package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var poem__0 string = "roses are red\nviolets are blue\n\"quotes\" stay quoted\nbackslash \\\\\\\\ stays too"
    var trailing_blank__1 string = "line one\n\nline three"
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(poem__0)
    _goml_runtime_core_string_println(inline184)
    var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(trailing_blank__1)
    _goml_runtime_core_string_println(inline181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
