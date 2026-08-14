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
    var inline223 struct{} = struct{}{}
    var inline224 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline223)
    _goml_runtime_core_string_print(inline224)
    var inline219 bool = true
    var inline220 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline219)
    _goml_runtime_core_string_print(inline220)
    var inline215 bool = false
    var inline216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline215)
    _goml_runtime_core_string_print(inline216)
    var inline211 int = 123
    var inline212 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline211)
    _goml_runtime_core_string_print(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t203 string = _goml_runtime_core_unit_to_string(self__63)
    return t203
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t206 string = _goml_runtime_core_bool_to_string(self__64)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t209 string = _goml_runtime_core_int_to_string(self__67)
    return t209
}

func main() {
    main0()
}
