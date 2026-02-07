pub const BUILTIN_PACKAGE: &str = "builtin";
pub const ROOT_PACKAGE: &str = "main";
pub const ENTRY_FUNCTION: &str = "main";
pub const ENTRY_WRAPPER_FUNCTION: &str = "main0";

pub fn is_builtin_package(package: &str) -> bool {
    package == BUILTIN_PACKAGE
}

pub fn is_root_package(package: &str) -> bool {
    package == ROOT_PACKAGE
}

pub fn is_special_unqualified_package(package: &str) -> bool {
    is_builtin_package(package) || is_root_package(package)
}
