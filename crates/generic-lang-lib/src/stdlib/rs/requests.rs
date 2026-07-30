//! The `requests` stdlib module: a blocking HTTP client over `ureq`.
//!
//! `get`/`post`/`put`/`delete` return a `Response` instance carrying
//! the status, headers, and body of the finished exchange. HTTP error
//! statuses are ordinary responses (`response.ok()` distinguishes);
//! only transport failures raise (`IoError`). Requests time out after
//! [`TIMEOUT`].

// Native functions must return `VmResult<Value>` to match the calling
// convention, so infallible ones still wrap in `Ok`.
#![allow(clippy::unnecessary_wraps)]

use std::sync::LazyLock;
use std::time::Duration;

use ureq::{Agent, RequestBuilder};

use crate::stdlib::rs::json::parse_json_source;
use crate::value::{Dict, Instance, ModuleContents, ModuleExport, NativeClass, Response, Value};
use crate::vm::ExceptionKind::{IoError, TypeError};
use crate::vm::VM;
use crate::vm::errors::{VmErrorKind, VmResult};

/// Every request times out (connect + read) after this long.
const TIMEOUT: Duration = Duration::from_secs(30);

/// The shared agent: HTTP statuses are data, not errors, and the
/// global timeout applies to every request.
static AGENT: LazyLock<Agent> = LazyLock::new(|| {
    Agent::config_builder()
        .http_status_as_error(false)
        .timeout_global(Some(TIMEOUT))
        .build()
        .into()
});

/// The HTTP methods the module offers; each knows whether it carries a
/// request body.
enum HttpMethod {
    Get,
    Delete,
    Post(String),
    Put(String),
}

impl HttpMethod {
    /// The lower-case name, for error messages.
    fn label(&self) -> &'static str {
        match self {
            Self::Get => "get",
            Self::Delete => "delete",
            Self::Post(_) => "post",
            Self::Put(_) => "put",
        }
    }
}

/// Read the `(name, value)` string pairs of a headers dict.
fn header_pairs(vm: &mut VM, headers: Value) -> VmResult<Vec<(String, String)>> {
    let entries: Vec<(Value, Value)> = match headers {
        Value::Instance(id) => match &id.to_value(&vm.heap).backing {
            Some(NativeClass::Dict(dict)) => dict
                .items
                .iter()
                .map(|(key, value, _)| (*key, *value))
                .collect(),
            _ => {
                return Err(headers_type_error(vm, headers));
            }
        },
        _ => {
            return Err(headers_type_error(vm, headers));
        }
    };
    let mut pairs = Vec::with_capacity(entries.len());
    for (key, value) in entries {
        let (Value::String(key_id), Value::String(value_id)) = (key, value) else {
            return Err(headers_type_error(vm, headers));
        };
        pairs.push((
            vm.heap.strings[key_id].clone(),
            vm.heap.strings[value_id].clone(),
        ));
    }
    Ok(pairs)
}

/// One consistent error for anything that is not a dict of strings to
/// strings.
fn headers_type_error(vm: &mut VM, headers: Value) -> VmErrorKind {
    let rendered = headers.to_string(&vm.heap);
    vm.throw(
        TypeError,
        &format!("headers must be a dict of strings to strings, got: {rendered}"),
    )
    .unwrap_err()
}

/// Apply the request headers to a builder of either body state.
fn with_headers<B>(
    mut builder: RequestBuilder<B>,
    headers: &[(String, String)],
) -> RequestBuilder<B> {
    for (name, value) in headers {
        builder = builder.header(name, value);
    }
    builder
}

/// Perform `method` (the body, if any, rides on the `Post`/`Put`
/// variant) and wrap the outcome as a `Response` instance. ureq types
/// with-body and without-body builders differently, so each method
/// drives its own build-and-send in one arm.
fn perform(
    vm: &mut VM,
    method: HttpMethod,
    url: &str,
    headers: &[(String, String)],
) -> VmResult<Value> {
    let result = match method {
        HttpMethod::Get => with_headers(AGENT.get(url), headers).call(),
        HttpMethod::Delete => with_headers(AGENT.delete(url), headers).call(),
        HttpMethod::Post(body) => with_headers(AGENT.post(url), headers).send(&body),
        HttpMethod::Put(body) => with_headers(AGENT.put(url), headers).send(&body),
    };
    let mut response = match result {
        Ok(response) => response,
        Err(error) => {
            return Err(vm
                .throw(IoError, &format!("Request to `{url}` failed: {error}"))
                .unwrap_err());
        }
    };
    let status = response.status().as_u16();
    let response_headers: Vec<(String, String)> = response
        .headers()
        .iter()
        .map(|(name, value)| {
            (
                name.as_str().to_string(),
                String::from_utf8_lossy(value.as_bytes()).into_owned(),
            )
        })
        .collect();
    let body = match response.body_mut().read_to_string() {
        Ok(body) => body,
        Err(error) => {
            return Err(vm
                .throw(
                    IoError,
                    &format!("Reading the response from `{url}` failed: {error}"),
                )
                .unwrap_err());
        }
    };
    Ok(make_response(vm, status, response_headers, body))
}

/// Build a `Response` instance from finished-exchange data.
fn make_response(vm: &mut VM, status: u16, headers: Vec<(String, String)>, body: String) -> Value {
    let instance = Instance::new(
        *vm.heap.native_classes.get("Response").unwrap(),
        Some(
            Response {
                status,
                headers,
                body,
            }
            .into(),
        ),
    );
    vm.heap.add_instance(instance)
}

/// The string at `args[index]`, or a `TypeError` naming `what`.
fn string_arg(
    vm: &mut VM,
    args: &[Value],
    index: usize,
    what: &str,
    label: &str,
) -> VmResult<String> {
    match args.get(index) {
        Some(Value::String(id)) => Ok(vm.heap.strings[*id].clone()),
        other => {
            let rendered =
                other.map_or_else(|| "nothing".to_string(), |value| value.to_string(&vm.heap));
            Err(vm
                .throw(
                    TypeError,
                    &format!("'{label}' expects a {what} string, got: {rendered}"),
                )
                .unwrap_err())
        }
    }
}

/// The optional headers dict at `args[index]`.
fn headers_arg(vm: &mut VM, args: &[Value], index: usize) -> VmResult<Vec<(String, String)>> {
    match args.get(index) {
        Some(&headers) => header_pairs(vm, headers),
        None => Ok(Vec::new()),
    }
}

/// A bodyless request (`get`/`delete`): `(url[, headers])`.
fn run_bodyless(vm: &mut VM, method: HttpMethod, args: &[Value]) -> VmResult<Value> {
    let url = string_arg(vm, args, 0, "url", method.label())?;
    let headers = headers_arg(vm, args, 1)?;
    perform(vm, method, &url, &headers)
}

/// A request with a body (`post`/`put`): `(url, body[, headers])`. The
/// body is parsed here and moved onto the method variant via `into`;
/// the label comes from that same variant (empty `String` does not
/// allocate), so the verb name is not repeated.
fn run_with_body(vm: &mut VM, into: fn(String) -> HttpMethod, args: &[Value]) -> VmResult<Value> {
    let label = into(String::new()).label();
    let url = string_arg(vm, args, 0, "url", label)?;
    let body = string_arg(vm, args, 1, "body", label)?;
    let headers = headers_arg(vm, args, 2)?;
    perform(vm, into(body), &url, &headers)
}

fn get_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_bodyless(vm, HttpMethod::Get, args)
}

fn delete_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_bodyless(vm, HttpMethod::Delete, args)
}

fn post_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_with_body(vm, HttpMethod::Post, args)
}

fn put_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    run_with_body(vm, HttpMethod::Put, args)
}

/// `Response.status_code()` - the HTTP status as an integer.
fn response_status_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(i64::from(receiver.as_response(&vm.heap).status).into())
}

/// `Response.ok()` - whether the status is below 400.
fn response_ok_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok((receiver.as_response(&vm.heap).status < 400).into())
}

/// `Response.text()` - the response body as a string.
fn response_text_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let body = receiver.as_response(&vm.heap).body.clone();
    Ok(vm.heap.string_id(&body).into())
}

/// `Response.headers()` - the response headers as a dict (header names
/// lowercased, as delivered).
fn response_headers_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let headers = receiver.as_response(&vm.heap).headers.clone();
    // The dict need not be rooted while it is filled: the keys are
    // strings, whose `__hash__`/`__eq__` are native and never re-enter
    // the VM, and allocation never collects within a single native
    // call.
    let dict = vm.new_dict();
    for (name, value) in headers {
        let key = vm.heap.string_id(&name).into();
        let value = vm.heap.string_id(&value).into();
        Dict::add(vm, &dict, key, value)?;
    }
    Ok(dict)
}

/// `Response.json()` - the body parsed as JSON.
fn response_json_native(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let body = receiver.as_response(&vm.heap).body.clone();
    parse_json_source(vm, &body)
}

/// `_response(status, body, headers)` - an internal helper that builds a
/// `Response` from given data, so the `.gen` tests can exercise the
/// `Response` methods without a network. Not part of the public surface
/// (the leading underscore marks it internal).
fn make_response_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::Number(crate::value::Number::Integer(status)) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'_response' expects an integer status, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let status = u16::try_from(status.to_bigint(&vm.heap)).unwrap_or(0);
    let Value::String(body_id) = args[1] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'_response' expects a body string, got: {}",
                    args[1].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let body = vm.heap.strings[body_id].clone();
    let headers = header_pairs(vm, args[2])?;
    Ok(make_response(vm, status, headers, body))
}

pub(super) fn register(vm: &mut VM) {
    vm.define_native_class(&"Response", false);
    vm.define_native_method(&"Response", &"status_code", &[0], response_status_native);
    vm.define_native_method(&"Response", &"ok", &[0], response_ok_native);
    vm.define_native_method(&"Response", &"text", &[0], response_text_native);
    vm.define_native_method(&"Response", &"headers", &[0], response_headers_native);
    vm.define_native_method(&"Response", &"json", &[0], response_json_native);
    vm.register_stdlib_module(&"requests", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "get",
            arity: &[1, 2],
            fun: get_native,
        },
        ModuleExport::Function {
            name: "post",
            arity: &[2, 3],
            fun: post_native,
        },
        ModuleExport::Function {
            name: "put",
            arity: &[2, 3],
            fun: put_native,
        },
        ModuleExport::Function {
            name: "_response",
            arity: &[3],
            fun: make_response_native,
        },
        ModuleExport::Function {
            name: "delete",
            arity: &[1, 2],
            fun: delete_native,
        },
        ModuleExport::Class { name: "Response" },
    ]
}

// Real network sockets: not runnable under `cargo miri test`.
#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::thread;

    /// A one-shot HTTP/1.1 server: accepts one connection, reads the
    /// request, and replies with `status`/`body`, echoing back the
    /// request's method and the `X-Probe` header so the test can assert
    /// what the client sent. Returns the bound `127.0.0.1` URL.
    fn serve_once(status: u16, body: &'static str) -> String {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let url = format!("http://{}/", listener.local_addr().unwrap());
        thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut buffer = [0_u8; 2048];
            let read = stream.read(&mut buffer).unwrap();
            let request = String::from_utf8_lossy(&buffer[..read]);
            let method = request.split_whitespace().next().unwrap_or("");
            let probe = request
                .lines()
                .find_map(|line| line.strip_prefix("x-probe: "))
                .or_else(|| {
                    request
                        .lines()
                        .find_map(|line| line.strip_prefix("X-Probe: "))
                })
                .unwrap_or("");
            let payload = format!("{body}|method={method}|probe={probe}");
            let response = format!(
                "HTTP/1.1 {status} X\r\nContent-Length: {}\r\nX-Server: probe\r\n\r\n{payload}",
                payload.len()
            );
            stream.write_all(response.as_bytes()).unwrap();
        });
        url
    }

    #[test]
    fn get_returns_status_headers_and_body() {
        let mut vm = VM::new();
        let url = serve_once(200, "hello");
        let url_value = Value::String(vm.heap.string_id(&url));
        let response = get_native(&mut vm, &[url_value]).unwrap();
        let response = response.as_response(&vm.heap);
        assert_eq!(response.status, 200);
        assert!(response.body.starts_with("hello|method=GET"));
        assert!(
            response
                .headers
                .iter()
                .any(|(name, value)| name.eq_ignore_ascii_case("x-server") && value == "probe")
        );
    }

    #[test]
    fn post_sends_body_and_header() {
        let mut vm = VM::new();
        let url = serve_once(201, "created");
        let url_value = Value::String(vm.heap.string_id(&url));
        let body_value = Value::String(vm.heap.string_id(&"payload".to_string()));
        // headers dict: {"X-Probe": "yes"}
        let headers = {
            let dict = vm.new_dict();
            let key = Value::String(vm.heap.string_id(&"X-Probe".to_string()));
            let value = Value::String(vm.heap.string_id(&"yes".to_string()));
            Dict::add(&mut vm, &dict, key, value).unwrap();
            dict
        };
        let response = post_native(&mut vm, &[url_value, body_value, headers]).unwrap();
        let response = response.as_response(&vm.heap);
        assert_eq!(response.status, 201);
        assert!(response.body.contains("method=POST"));
        assert!(response.body.contains("probe=yes"));
    }

    #[test]
    fn error_status_is_a_response_not_a_throw() {
        let mut vm = VM::new();
        let url = serve_once(404, "nope");
        let url_value = Value::String(vm.heap.string_id(&url));
        let response = get_native(&mut vm, &[url_value]).unwrap();
        let response = response.as_response(&vm.heap);
        assert_eq!(response.status, 404);
    }

    #[test]
    fn transport_failure_raises() {
        let mut vm = VM::new();
        // Nothing listening on this port.
        let url_value = Value::String(vm.heap.string_id(&"http://127.0.0.1:1/".to_string()));
        assert!(get_native(&mut vm, &[url_value]).is_err());
    }

    #[test]
    fn non_string_url_is_a_type_error() {
        let mut vm = VM::new();
        assert!(get_native(&mut vm, &[Value::Nil]).is_err());
    }
}
