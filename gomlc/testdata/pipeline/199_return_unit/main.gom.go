package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var inline198 string = "continued"
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline198)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
