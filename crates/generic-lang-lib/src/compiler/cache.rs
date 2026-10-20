//! Disk cache for compiled imported modules.
//!
//! Cache files contain executable bytecode and are trusted local state.
//! Debug builds disable the cache by default; release builds key entries by
//! the compiler's major.minor version, so bytecode-incompatible compiler
//! changes must bump the minor version.

use crate::{
    chunk::{Chunk, CodeOffset},
    heap::Heap,
    types::{Column, Line, Location, OpcodeLocation},
    value::{Function, GenericInt, GenericRational, Instance, NativeClass, Number, Tuple, Value},
};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_bigint::BigInt;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::{
    fs::{self, Metadata},
    io::{self, Cursor, Write},
    path::{Path, PathBuf},
    time::UNIX_EPOCH,
};
use tempfile::NamedTempFile;

const MAGIC: &[u8; 8] = b"GENCACHE";
const CACHE_VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION_MAJOR"),
    ".",
    env!("CARGO_PKG_VERSION_MINOR")
);
const CACHE_DIRECTORY: &str = "__generic_cache__";
/// Bounds recursive cache encoding/decoding.
const MAX_CACHE_NESTING: usize = 2000;

#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum ValueTag {
    Nil,
    Bool,
    StopIteration,
    Integer,
    Float,
    Rational,
    String,
    Function,
    Tuple,
}

#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum IntTag {
    Small,
    Big,
}

impl Value {
    fn cache_tag(self, heap: &Heap) -> Option<ValueTag> {
        match self {
            Self::Nil => Some(ValueTag::Nil),
            Self::Bool(_) => Some(ValueTag::Bool),
            Self::StopIteration => Some(ValueTag::StopIteration),
            Self::Number(Number::Integer(_)) => Some(ValueTag::Integer),
            Self::Number(Number::Float(_)) => Some(ValueTag::Float),
            Self::Number(Number::Rational(_)) => Some(ValueTag::Rational),
            Self::String(_) => Some(ValueTag::String),
            Self::Function(_) => Some(ValueTag::Function),
            Self::Instance(id) => match &id.to_value(heap).backing {
                Some(NativeClass::Tuple(_)) => Some(ValueTag::Tuple),
                _ => {
                    eprintln!(
                        "module cache: cannot encode unsupported instance constant; \
                         skipping cache. Use --no-cache to disable the cache."
                    );
                    None
                }
            },
            other => {
                eprintln!(
                    "module cache: cannot encode unsupported constant value {other:?}; \
                     skipping cache. Use --no-cache to disable the cache."
                );
                None
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SourceInfo {
    size: u64,
    mtime_secs: u64,
    mtime_nanos: u32,
}

impl SourceInfo {
    fn new(source: &str, metadata: &Metadata) -> Option<Self> {
        let modified = metadata.modified().ok()?;
        let elapsed = modified.duration_since(UNIX_EPOCH).ok()?;
        Some(Self {
            size: u64::try_from(source.len()).ok()?,
            mtime_secs: elapsed.as_secs(),
            mtime_nanos: elapsed.subsec_nanos(),
        })
    }

    fn encode(self, out: &mut Vec<u8>) -> Option<()> {
        out.write_u64::<LittleEndian>(self.size).ok()?;
        out.write_u64::<LittleEndian>(self.mtime_secs).ok()?;
        out.write_u32::<LittleEndian>(self.mtime_nanos).ok()?;
        Some(())
    }

    fn decode(reader: &mut Cursor<&[u8]>) -> Option<Self> {
        Some(Self {
            size: reader.read_u64::<LittleEndian>().ok()?,
            mtime_secs: reader.read_u64::<LittleEndian>().ok()?,
            mtime_nanos: reader.read_u32::<LittleEndian>().ok()?,
        })
    }
}

pub(crate) fn load(
    source_path: &Path,
    source: &str,
    metadata: &Metadata,
    heap: &mut Heap,
) -> Option<Function> {
    let path = cache_path(source_path)?;
    let bytes = fs::read(path).ok()?;
    let mut reader = Cursor::new(bytes.as_slice());

    if read_bytes(&mut reader, MAGIC.len())? != MAGIC {
        return None;
    }
    if read_string(&mut reader)? != CACHE_VERSION {
        return None;
    }
    if SourceInfo::decode(&mut reader)? != SourceInfo::new(source, metadata)? {
        return None;
    }

    let function = decode_function(&mut reader, heap, 0)?;
    if reader.position() != u64::try_from(bytes.len()).ok()? {
        return None;
    }
    Some(function)
}

pub(crate) fn store(
    source_path: &Path,
    source: &str,
    metadata: &Metadata,
    function: &Function,
    heap: &Heap,
) {
    let Some(bytes) = build_cache_content(source, metadata, function, heap) else {
        return;
    };
    let Some(path) = cache_path(source_path) else {
        return;
    };
    // Cache writes are best-effort; a failed write must not fail the import.
    drop(write_atomic(&path, &bytes));
}

fn build_cache_content(
    source: &str,
    metadata: &Metadata,
    function: &Function,
    heap: &Heap,
) -> Option<Vec<u8>> {
    let source_info = SourceInfo::new(source, metadata)?;

    let mut out = Vec::new();
    out.extend_from_slice(MAGIC);
    write_string(&mut out, CACHE_VERSION)?;
    source_info.encode(&mut out)?;
    encode_function(&mut out, function, heap, 0)?;
    Some(out)
}

fn cache_path(source_path: &Path) -> Option<PathBuf> {
    let canonical = source_path
        .canonicalize()
        .unwrap_or_else(|_| source_path.to_path_buf());
    let parent = canonical.parent()?;
    let file_name = canonical.file_name()?.to_string_lossy();

    Some(
        parent
            .join(CACHE_DIRECTORY)
            .join(format!("{file_name}.generic-{CACHE_VERSION}.genc")),
    )
}

fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| io::Error::other("cache path has no parent directory"))?;
    fs::create_dir_all(parent)?;

    let mut temporary = NamedTempFile::new_in(parent)?;
    temporary.write_all(bytes)?;
    temporary.persist(path).map_err(|error| error.error)?;
    Ok(())
}

fn encode_function(
    out: &mut Vec<u8>,
    function: &Function,
    heap: &Heap,
    depth: usize,
) -> Option<()> {
    encode_function_with_limit(out, function, heap, depth, MAX_CACHE_NESTING)
}

fn encode_function_with_limit(
    out: &mut Vec<u8>,
    function: &Function,
    heap: &Heap,
    depth: usize,
    limit: usize,
) -> Option<()> {
    if depth > limit {
        eprintln!(
            "module cache: nesting limit of {limit} exceeded; \
             skipping cache. Use --no-cache to disable the cache."
        );
        return None;
    }

    write_usize(out, function.arity)?;
    write_usize(out, function.required_params)?;
    out.write_u8(u8::from(function.is_variadic)).ok()?;
    write_usize(out, function.upvalue_count)?;
    write_string(out, function.name.to_value(heap))?;
    write_string(out, function.chunk.name().to_value(heap))?;

    let code = function.chunk.code();
    write_len(out, code.len())?;
    out.write_all(code).ok()?;
    for index in 0..code.len() {
        encode_opcode_location(out, function.chunk.get_location(CodeOffset(index)))?;
    }

    let constants = function.chunk.constants();
    write_len(out, constants.len())?;
    for constant in constants {
        encode_value_with_limit(out, *constant, heap, depth + 1, limit)?;
    }
    Some(())
}

fn decode_function(reader: &mut Cursor<&[u8]>, heap: &mut Heap, depth: usize) -> Option<Function> {
    decode_function_with_limit(reader, heap, depth, MAX_CACHE_NESTING)
}

fn decode_function_with_limit(
    reader: &mut Cursor<&[u8]>,
    heap: &mut Heap,
    depth: usize,
    limit: usize,
) -> Option<Function> {
    if depth > limit {
        return None;
    }

    let arity = read_usize(reader)?;
    let required_params = read_usize(reader)?;
    let is_variadic = read_bool(reader)?;
    let upvalue_count = read_usize(reader)?;
    let name = read_string(reader)?;
    let chunk_name = read_string(reader)?;
    let name = heap.string_id(&name);
    let chunk_name = heap.string_id(&chunk_name);

    let code_len = read_len(reader)?;
    let code = read_bytes(reader, code_len)?.to_vec();
    let mut chunk = Chunk::new(chunk_name);
    for byte in code {
        let location = decode_opcode_location(reader)?;
        chunk.write(byte, location);
    }

    let constant_count = read_len(reader)?;
    for _ in 0..constant_count {
        chunk.make_constant(decode_value_with_limit(reader, heap, depth + 1, limit)?);
    }

    Some(Function {
        arity,
        required_params,
        is_variadic,
        chunk,
        name,
        upvalue_count,
    })
}

fn encode_value_with_limit(
    out: &mut Vec<u8>,
    value: Value,
    heap: &Heap,
    depth: usize,
    limit: usize,
) -> Option<()> {
    if depth > limit {
        eprintln!(
            "module cache: nesting limit of {limit} exceeded; \
             skipping cache. Use --no-cache to disable the cache."
        );
        return None;
    }

    out.write_u8(value.cache_tag(heap)?.into()).ok()?;
    match value {
        Value::Nil | Value::StopIteration => {}
        Value::Bool(value) => out.write_u8(u8::from(value)).ok()?,
        Value::Number(Number::Integer(value)) => encode_int(out, value, heap)?,
        Value::Number(Number::Float(value)) => {
            out.write_u64::<LittleEndian>(value.to_bits()).ok()?;
        }
        Value::Number(Number::Rational(value)) => {
            encode_int(out, value.numerator(), heap)?;
            encode_int(out, value.denominator(), heap)?;
        }
        Value::String(id) => write_string(out, id.to_value(heap))?,
        Value::Function(id) => {
            encode_function_with_limit(out, id.to_value(heap), heap, depth, limit)?;
        }
        Value::Instance(id) => {
            let Some(NativeClass::Tuple(tuple)) = &id.to_value(heap).backing else {
                unreachable!("cache_tag validated the tuple instance");
            };
            write_len(out, tuple.items().len())?;
            for item in tuple.items() {
                encode_value_with_limit(out, *item, heap, depth + 1, limit)?;
            }
        }
        _ => unreachable!("cache_tag only returns tags for supported values"),
    }
    Some(())
}

fn decode_value_with_limit(
    reader: &mut Cursor<&[u8]>,
    heap: &mut Heap,
    depth: usize,
    limit: usize,
) -> Option<Value> {
    if depth > limit {
        return None;
    }

    let tag = ValueTag::try_from(reader.read_u8().ok()?).ok()?;
    Some(match tag {
        ValueTag::Nil => Value::Nil,
        ValueTag::Bool => Value::Bool(read_bool(reader)?),
        ValueTag::StopIteration => Value::StopIteration,
        ValueTag::Integer => Value::Number(Number::Integer(decode_int(reader, heap)?)),
        ValueTag::Float => Value::Number(Number::Float(f64::from_bits(
            reader.read_u64::<LittleEndian>().ok()?,
        ))),
        ValueTag::Rational => Value::Number(Number::Rational(
            GenericRational::new(decode_int(reader, heap)?, decode_int(reader, heap)?, heap)
                .ok()?,
        )),
        ValueTag::String => Value::String(heap.string_id(&read_string(reader)?)),
        ValueTag::Function => {
            let function = decode_function_with_limit(reader, heap, depth, limit)?;
            heap.add_function(function)
        }
        ValueTag::Tuple => {
            let count = read_len(reader)?;
            let mut items = Vec::new();
            for _ in 0..count {
                items.push(decode_value_with_limit(reader, heap, depth + 1, limit)?);
            }
            let class = *heap
                .native_classes
                .get("Tuple")
                .expect("Tuple native class must exist");
            let instance = Instance::new(class, Some(Tuple::new(items).into()));
            heap.add_instance(instance)
        }
    })
}

fn encode_int(out: &mut Vec<u8>, value: GenericInt, heap: &Heap) -> Option<()> {
    match value {
        GenericInt::Small(value) => {
            out.write_u8(IntTag::Small.into()).ok()?;
            out.write_i64::<LittleEndian>(value).ok()?;
        }
        GenericInt::Big(id) => {
            out.write_u8(IntTag::Big.into()).ok()?;
            write_bytes(out, &id.to_value(heap).to_signed_bytes_le())?;
        }
    }
    Some(())
}

fn decode_int(reader: &mut Cursor<&[u8]>, heap: &mut Heap) -> Option<GenericInt> {
    Some(match IntTag::try_from(reader.read_u8().ok()?).ok()? {
        IntTag::Small => GenericInt::Small(reader.read_i64::<LittleEndian>().ok()?),
        IntTag::Big => {
            let len = read_len(reader)?;
            let bytes = read_bytes(reader, len)?;
            *heap
                .add_big_int(BigInt::from_signed_bytes_le(bytes))
                .as_generic_int()
        }
    })
}

fn encode_opcode_location(out: &mut Vec<u8>, location: OpcodeLocation) -> Option<()> {
    encode_optional_location(out, location.preceding)?;
    encode_location(out, location.source)?;
    encode_optional_location(out, location.following)
}

fn decode_opcode_location(reader: &mut Cursor<&[u8]>) -> Option<OpcodeLocation> {
    let preceding = decode_optional_location(reader)?;
    let source = decode_location(reader)?;
    let following = decode_optional_location(reader)?;
    Some(OpcodeLocation {
        preceding,
        source,
        following,
    })
}

fn encode_optional_location(out: &mut Vec<u8>, location: Option<Location>) -> Option<()> {
    match location {
        Some(location) => {
            out.write_u8(1).ok()?;
            encode_location(out, location)
        }
        None => {
            out.write_u8(0).ok()?;
            Some(())
        }
    }
}

fn decode_optional_location(reader: &mut Cursor<&[u8]>) -> Option<Option<Location>> {
    match reader.read_u8().ok()? {
        0 => Some(None),
        1 => Some(Some(decode_location(reader)?)),
        _ => None,
    }
}

fn encode_location(out: &mut Vec<u8>, location: Location) -> Option<()> {
    write_usize(out, *location.start_line)?;
    write_usize(out, *location.start_column)?;
    write_usize(out, *location.end_line)?;
    write_usize(out, *location.end_column)?;
    write_usize(out, location.index)
}

fn decode_location(reader: &mut Cursor<&[u8]>) -> Option<Location> {
    Some(Location {
        start_line: Line(read_usize(reader)?),
        start_column: Column(read_usize(reader)?),
        end_line: Line(read_usize(reader)?),
        end_column: Column(read_usize(reader)?),
        index: read_usize(reader)?,
    })
}

fn write_bytes(out: &mut Vec<u8>, bytes: &[u8]) -> Option<()> {
    write_len(out, bytes.len())?;
    out.write_all(bytes).ok()?;
    Some(())
}

fn write_string(out: &mut Vec<u8>, value: &str) -> Option<()> {
    write_bytes(out, value.as_bytes())
}

fn write_usize(out: &mut Vec<u8>, value: usize) -> Option<()> {
    out.write_u64::<LittleEndian>(u64::try_from(value).ok()?)
        .ok()?;
    Some(())
}

fn write_len(out: &mut Vec<u8>, value: usize) -> Option<()> {
    write_usize(out, value)
}

fn read_bytes<'a>(reader: &mut Cursor<&'a [u8]>, len: usize) -> Option<&'a [u8]> {
    let start = usize::try_from(reader.position()).ok()?;
    let end = start.checked_add(len)?;
    let bytes = reader.get_ref().get(start..end)?;
    reader.set_position(u64::try_from(end).ok()?);
    Some(bytes)
}

fn read_len(reader: &mut Cursor<&[u8]>) -> Option<usize> {
    let len = usize::try_from(reader.read_u64::<LittleEndian>().ok()?).ok()?;
    let position = usize::try_from(reader.position()).ok()?;
    let remaining = reader.get_ref().len().checked_sub(position)?;
    (len <= remaining).then_some(len)
}

fn read_string(reader: &mut Cursor<&[u8]>) -> Option<String> {
    let len = read_len(reader)?;
    String::from_utf8(read_bytes(reader, len)?.to_vec()).ok()
}

fn read_bool(reader: &mut Cursor<&[u8]>) -> Option<bool> {
    match reader.read_u8().ok()? {
        0 => Some(false),
        1 => Some(true),
        _ => None,
    }
}

fn read_usize(reader: &mut Cursor<&[u8]>) -> Option<usize> {
    usize::try_from(reader.read_u64::<LittleEndian>().ok()?).ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        chunk::OpCode,
        types::OpcodeLocation,
        value::{Class, ClassKind, Upvalue},
    };
    use num_bigint::BigInt;

    fn test_location(line: usize, index: usize) -> OpcodeLocation {
        OpcodeLocation {
            preceding: Some(Location {
                start_line: Line(line.saturating_sub(1)),
                start_column: Column(1),
                end_line: Line(line),
                end_column: Column(2),
                index,
            }),
            source: Location {
                start_line: Line(line),
                start_column: Column(3),
                end_line: Line(line),
                end_column: Column(9),
                index,
            },
            following: None,
        }
    }

    fn make_leaf_function(heap: &mut Heap) -> Function {
        let name = heap.string_id(&"leaf");
        let mut chunk = Chunk::new(name);
        chunk.write(OpCode::Nil, test_location(2, 10));
        chunk.write(OpCode::Return, test_location(2, 11));
        chunk.make_constant(heap.string_id(&"leaf constant").into());
        Function {
            arity: 0,
            required_params: 0,
            is_variadic: false,
            chunk,
            name,
            upvalue_count: 0,
        }
    }

    /// Build `depth` real nested functions around a leaf function.
    fn make_nested_chain(heap: &mut Heap, depth: usize) -> Function {
        let mut current = make_leaf_function(heap);
        for _ in 0..depth {
            let name = heap.string_id(&"nested chain");
            let mut chunk = Chunk::new(name);
            let child = heap.add_function(current);
            chunk.make_constant(child);
            chunk.write(OpCode::Return, test_location(1, 0));
            current = Function {
                arity: 0,
                required_params: 0,
                is_variadic: false,
                chunk,
                name,
                upvalue_count: 0,
            };
        }
        current
    }

    fn make_function(heap: &mut Heap) -> Function {
        let name = heap.string_id(&"cached module");
        let mut chunk = Chunk::new(name);

        chunk.write(OpCode::Constant, test_location(1, 0));
        chunk.make_constant(heap.string_id(&"hello").into());
        chunk.write(OpCode::Return, test_location(1, 12));

        chunk.make_constant(Value::Bool(true));
        chunk.make_constant(Value::Nil);
        chunk.make_constant(Value::StopIteration);
        chunk.make_constant(Value::from(-7_i64));
        chunk.make_constant(Value::from(
            *heap
                .add_big_int(BigInt::from(123_456_789_012_345_678_901_i128))
                .as_generic_int(),
        ));
        chunk.make_constant(Value::from(1.25_f64));
        chunk.make_constant(Value::Number(Number::Rational(
            GenericRational::new(GenericInt::Small(3), GenericInt::Small(7), heap).unwrap(),
        )));

        let nested = make_leaf_function(heap);
        let nested = heap.add_function(nested);
        chunk.make_constant(nested);

        Function {
            arity: 2,
            required_params: 1,
            is_variadic: true,
            chunk,
            name,
            upvalue_count: 3,
        }
    }

    #[test]
    fn function_roundtrip_rebuilds_into_a_fresh_heap() {
        let mut heap = Heap::new();
        let function = make_function(&mut heap);
        let mut encoded = Vec::new();
        encode_function(&mut encoded, &function, &heap, 0).unwrap();

        let mut other_heap = Heap::new();
        let mut reader = Cursor::new(encoded.as_slice());
        let restored = decode_function(&mut reader, &mut other_heap, 0).unwrap();
        assert_eq!(reader.position(), u64::try_from(encoded.len()).unwrap());

        let mut reencoded = Vec::new();
        encode_function(&mut reencoded, &restored, &other_heap, 0).unwrap();
        assert_eq!(encoded, reencoded);
    }

    #[test]
    fn cache_rejects_excessive_nesting_on_encode() {
        let mut heap = Heap::new();
        let function = make_nested_chain(&mut heap, 3);
        let mut encoded = Vec::new();
        assert!(encode_function_with_limit(&mut encoded, &function, &heap, 0, 2).is_none());
    }

    #[test]
    fn cache_rejects_excessive_nesting_on_decode() {
        let mut heap = Heap::new();
        let function = make_nested_chain(&mut heap, 3);
        let mut encoded = Vec::new();
        encode_function_with_limit(&mut encoded, &function, &heap, 0, 10).unwrap();

        let mut reader = Cursor::new(encoded.as_slice());
        let mut loaded_heap = Heap::new();
        assert!(decode_function_with_limit(&mut reader, &mut loaded_heap, 0, 2).is_none());
    }

    #[test]
    fn unsupported_constants_skip_caching() {
        let mut heap = Heap::new();
        let name = heap.string_id(&"unsupported");
        let mut chunk = Chunk::new(name);
        chunk.make_constant(heap.add_upvalue(Upvalue::Open(0)));
        let function = Function {
            arity: 0,
            required_params: 0,
            is_variadic: false,
            chunk,
            name,
            upvalue_count: 0,
        };

        let mut encoded = Vec::new();
        assert!(encode_function(&mut encoded, &function, &heap, 0).is_none());
    }

    fn register_tuple_class(heap: &mut Heap) -> crate::heap::ClassId {
        let name = heap.string_id(&"Tuple");
        let class = *heap
            .add_class(Class::new(name, ClassKind::Native))
            .as_class();
        heap.native_classes.insert("Tuple".to_string(), class);
        class
    }

    fn make_nested_tuple(heap: &mut Heap, class: crate::heap::ClassId, depth: usize) -> Value {
        let mut value = Value::Nil;
        for _ in 0..depth {
            let tuple = Tuple::new(vec![value]);
            let instance = Instance::new(class, Some(tuple.into()));
            value = heap.add_instance(instance);
        }
        value
    }

    fn make_function_with_constant(heap: &mut Heap, value: Value) -> Function {
        let name = heap.string_id(&"tuple constant");
        let mut chunk = Chunk::new(name);
        chunk.make_constant(value);
        chunk.write(OpCode::Return, test_location(1, 0));
        Function {
            arity: 0,
            required_params: 0,
            is_variadic: false,
            chunk,
            name,
            upvalue_count: 0,
        }
    }

    #[test]
    fn tuple_nesting_counts_toward_the_limit() {
        let mut heap = Heap::new();
        let class = register_tuple_class(&mut heap);
        let value = make_nested_tuple(&mut heap, class, 3);
        let function = make_function_with_constant(&mut heap, value);

        let mut encoded = Vec::new();
        assert!(encode_function_with_limit(&mut encoded, &function, &heap, 0, 2).is_none());

        let mut encoded = Vec::new();
        encode_function_with_limit(&mut encoded, &function, &heap, 0, 10).unwrap();
        let mut reader = Cursor::new(encoded.as_slice());
        let mut loaded_heap = Heap::new();
        register_tuple_class(&mut loaded_heap);
        assert!(decode_function_with_limit(&mut reader, &mut loaded_heap, 0, 2).is_none());
    }

    #[test]
    fn disk_cache_roundtrips() {
        let directory = tempfile::tempdir().unwrap();
        let source_path = directory.path().join("module.gen");
        let source = "var value = 1;\n";
        fs::write(&source_path, source).unwrap();
        let metadata = fs::metadata(&source_path).unwrap();

        // Write the cache, then decode it into a fresh heap.
        let mut heap = Heap::new();
        let function = make_function(&mut heap);
        store(&source_path, source, &metadata, &function, &heap);
        let cache_path = cache_path(&source_path).unwrap();
        assert!(cache_path.is_file());

        let mut loaded_heap = Heap::new();
        let loaded = load(&source_path, source, &metadata, &mut loaded_heap).unwrap();
        assert_eq!(loaded.arity, function.arity);
    }

    #[test]
    fn corrupt_disk_cache_is_ignored() {
        let directory = tempfile::tempdir().unwrap();
        let source_path = directory.path().join("module.gen");
        let source = "var value = 1;\n";
        fs::write(&source_path, source).unwrap();
        let metadata = fs::metadata(&source_path).unwrap();

        // Malformed cache bytes must be a miss, not a panic.
        let mut heap = Heap::new();
        let function = make_function(&mut heap);
        store(&source_path, source, &metadata, &function, &heap);
        let cache_path = cache_path(&source_path).unwrap();
        fs::write(&cache_path, b"invalid cache").unwrap();

        let mut invalid_heap = Heap::new();
        assert!(load(&source_path, source, &metadata, &mut invalid_heap).is_none());
    }

    #[test]
    fn changed_source_invalidates_disk_cache() {
        let directory = tempfile::tempdir().unwrap();
        let source_path = directory.path().join("module.gen");
        let source = "var value = 1;\n";
        fs::write(&source_path, source).unwrap();
        let metadata = fs::metadata(&source_path).unwrap();

        // Store against the original source, then change its size and mtime.
        let mut heap = Heap::new();
        let function = make_function(&mut heap);
        store(&source_path, source, &metadata, &function, &heap);

        let changed = "var value = 123;\n";
        fs::write(&source_path, changed).unwrap();
        let changed_metadata = fs::metadata(&source_path).unwrap();
        let mut stale_heap = Heap::new();
        assert!(load(&source_path, changed, &changed_metadata, &mut stale_heap).is_none());
    }
}
