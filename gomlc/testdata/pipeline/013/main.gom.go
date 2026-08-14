package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var s__0 string = "abcde"
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_println(inline202)
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_print(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
