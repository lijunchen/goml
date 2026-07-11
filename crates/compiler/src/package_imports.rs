use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct ExternalImports {
    pub package_names: HashMap<String, String>,
}

impl ExternalImports {
    pub fn new(package_names: HashMap<String, String>) -> Self {
        Self { package_names }
    }

    pub fn contains_package(&self, package: &str) -> bool {
        self.package_names.contains_key(package)
    }

    pub fn declared_name(&self, package: &str) -> Option<&str> {
        self.package_names.get(package).map(String::as_str)
    }
}
