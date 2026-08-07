package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    var inline208 struct{} = struct{}{}
    var inline209 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline208)
    _goml_runtime_core_string_print(inline209)
    var inline204 bool = true
    var inline205 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline204)
    _goml_runtime_core_string_print(inline205)
    var inline200 bool = false
    var inline201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline200)
    _goml_runtime_core_string_print(inline201)
    var inline196 int = 123
    var inline197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline196)
    _goml_runtime_core_string_print(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t188 string = _goml_runtime_core_unit_to_string(self__65)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t191 string = _goml_runtime_core_bool_to_string(self__66)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t194 string = _goml_runtime_core_int_to_string(self__69)
    return t194
}

func main() {
    main0()
}
