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
    var inline213 struct{} = struct{}{}
    var inline214 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline213)
    _goml_runtime_core_string_print(inline214)
    var inline209 bool = true
    var inline210 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline209)
    _goml_runtime_core_string_print(inline210)
    var inline205 bool = false
    var inline206 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline205)
    _goml_runtime_core_string_print(inline206)
    var inline201 int = 123
    var inline202 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline201)
    _goml_runtime_core_string_print(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t193 string = _goml_runtime_core_unit_to_string(self__65)
    return t193
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t196 string = _goml_runtime_core_bool_to_string(self__66)
    return t196
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t199 string = _goml_runtime_core_int_to_string(self__69)
    return t199
}

func main() {
    main0()
}
