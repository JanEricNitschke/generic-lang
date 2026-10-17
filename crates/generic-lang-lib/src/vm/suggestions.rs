//! Suggestion-enriched error messages for the VM.
//!
//! The tables and the edit-distance matching are pure and live in
//! [`crate::suggestions`]; this module is the glue that turns VM state
//! (globals, attribute chains, module exports) into candidate pools and
//! finished message strings. The gathering helpers here run only on
//! error paths, so they may afford to walk the heap and allocate freely.

use super::VM;
use crate::config::GENERIC_STDLIB_DIR;
use crate::heap::{ClassId, ModuleId, StringId};
use crate::suggestions::{
    did_you_mean, did_you_mean_suffix, method_alias_hint, other_language_name_hint,
};
use crate::value::Value;

impl VM {
    /// `Undefined variable 'name'.` plus a suggestion drawn from the
    /// globals of the defining module and the builtins, or - when
    /// nothing is close - a hint for builtin names that only exist in
    /// other languages (`None` -> `nil`).
    pub(super) fn undefined_variable_message(&self, name: StringId) -> String {
        let name = name.to_value(&self.heap);
        let mut candidates: Vec<&str> = self
            .defining_module()
            .to_value(&self.heap)
            .globals
            .keys()
            .map(|global| global.to_value(&self.heap).as_str())
            .collect();
        candidates.extend(
            self.builtins
                .keys()
                .map(|builtin| builtin.to_value(&self.heap).as_str()),
        );
        let suffix = match did_you_mean(name, &candidates) {
            Some(candidate) => format!(" Did you mean: '{candidate}'?"),
            None => other_language_name_hint(name)
                .map(|hint| format!(" {hint}"))
                .unwrap_or_default(),
        };
        format!("Undefined variable '{name}'.{suffix}")
    }

    /// `Undefined property 'field'.` plus a suggestion drawn from the
    /// attribute names reachable on the receiver (fields, methods, and
    /// class variables, including inherited ones). For builtin types
    /// with no close match, a hint for common other-language method
    /// names (`push` -> `append`) is appended instead.
    ///
    /// Candidates are gathered in the order attribute lookup itself
    /// uses: instance fields first, then methods and class variables
    /// from the class itself before its ancestors. An equal-distance
    /// tie therefore resolves the way lookup would - a field wins over
    /// a method, a subclass member over an inherited one.
    pub(crate) fn undefined_property_message(&self, receiver: Value, field: StringId) -> String {
        let field = field.to_value(&self.heap);
        let mut candidates: Vec<&str> = Vec::new();
        let native_class_name = match receiver {
            Value::Instance(instance) => {
                candidates.extend(
                    instance
                        .to_value(&self.heap)
                        .fields
                        .keys()
                        .map(String::as_str),
                );
                let class = instance.to_value(&self.heap).class;
                self.collect_class_attributes(class, &mut candidates);
                class
                    .to_value(&self.heap)
                    .get_native_superclass(&self.heap, class)
                    .map(|native| {
                        native
                            .to_value(&self.heap)
                            .name
                            .to_value(&self.heap)
                            .as_str()
                    })
            }
            Value::Class(class) => {
                self.collect_class_attributes(class, &mut candidates);
                class
                    .to_value(&self.heap)
                    .get_native_superclass(&self.heap, class)
                    .map(|native| {
                        native
                            .to_value(&self.heap)
                            .name
                            .to_value(&self.heap)
                            .as_str()
                    })
            }
            _ => None,
        };

        let suffix = match did_you_mean(field, &candidates) {
            Some(candidate) => format!(" Did you mean: '{candidate}'?"),
            None => native_class_name
                .and_then(|class_name| method_alias_hint(class_name, field))
                .map(|hint| format!(" {hint}"))
                .unwrap_or_default(),
        };
        format!("Undefined property '{field}'.{suffix}")
    }

    /// `Undefined name 'field' in module m.` plus a suggestion drawn
    /// from the module's globals.
    pub(crate) fn undefined_module_name_message(
        &self,
        module: ModuleId,
        field: StringId,
    ) -> String {
        let field = field.to_value(&self.heap);
        let module_name = module.to_value(&self.heap).name.to_value(&self.heap);
        let candidates: Vec<&str> = module
            .to_value(&self.heap)
            .globals
            .keys()
            .map(|global| global.to_value(&self.heap).as_str())
            .collect();
        format!(
            "Undefined name '{field}' in module {module_name}.{}",
            did_you_mean_suffix(field, &candidates)
        )
    }

    /// The "Could not find name to import" message plus a suggestion
    /// drawn from the names the module actually exports.
    pub(super) fn missing_import_name_message(&self, module: ModuleId, name: StringId) -> String {
        let name = name.to_value(&self.heap);
        let candidates: Vec<&str> = module
            .to_value(&self.heap)
            .globals
            .keys()
            .map(|global| global.to_value(&self.heap).as_str())
            .collect();
        format!(
            "Could not find name to import `{name}`.{}",
            did_you_mean_suffix(name, &candidates)
        )
    }

    /// ` Did you mean: 'collections'?` for a failed import, naming the
    /// closest stdlib module; empty when nothing is close.
    ///
    /// The rust stdlib registers its modules by name (`self.stdlib`);
    /// the generic stdlib is embedded as one file per module, so its
    /// candidate names come from the embedded directory. The union of
    /// both pools is the suggestion base.
    pub(super) fn stdlib_import_suggestion(&self, name: &str) -> String {
        let mut candidates: Vec<&str> = self
            .stdlib
            .keys()
            .map(|key| key.to_value(&self.heap).as_str())
            .collect();
        for file in GENERIC_STDLIB_DIR.files() {
            if let Some(stem) = file.path().file_stem().and_then(|stem| stem.to_str()) {
                candidates.push(stem);
            }
        }
        did_you_mean_suffix(name, &candidates)
    }

    /// Every attribute name reachable on `class` or its ancestors:
    /// methods and class variables.
    fn collect_class_attributes<'a>(&'a self, mut class: ClassId, candidates: &mut Vec<&'a str>) {
        loop {
            let data = class.to_value(&self.heap);
            candidates.extend(
                data.methods
                    .keys()
                    .map(|method| method.to_value(&self.heap).as_str()),
            );
            candidates.extend(
                data.variables
                    .keys()
                    .map(|variable| variable.to_value(&self.heap).as_str()),
            );
            match data.superclass {
                Some(superclass) => class = superclass,
                None => return,
            }
        }
    }
}
