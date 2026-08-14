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
    var inline218 struct{} = struct{}{}
    var inline219 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline218)
    _goml_runtime_core_string_print(inline219)
    var inline214 bool = true
    var inline215 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline214)
    _goml_runtime_core_string_print(inline215)
    var inline210 bool = false
    var inline211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline210)
    _goml_runtime_core_string_print(inline211)
    var inline206 int = 123
    var inline207 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline206)
    _goml_runtime_core_string_print(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t198 string = _goml_runtime_core_unit_to_string(self__63)
    return t198
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t201 string = _goml_runtime_core_bool_to_string(self__64)
    return t201
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t204 string = _goml_runtime_core_int_to_string(self__67)
    return t204
}

func main() {
    main0()
}
