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
    var inline172 struct{} = struct{}{}
    var inline173 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline172)
    _goml_runtime_core_string_print(inline173)
    var inline168 bool = true
    var inline169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline168)
    _goml_runtime_core_string_print(inline169)
    var inline164 bool = false
    var inline165 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline164)
    _goml_runtime_core_string_print(inline165)
    var inline160 int = 123
    var inline161 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline160)
    _goml_runtime_core_string_print(inline161)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t152 string = _goml_runtime_core_unit_to_string(self__65)
    return t152
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t155 string = _goml_runtime_core_bool_to_string(self__66)
    return t155
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t158 string = _goml_runtime_core_int_to_string(self__69)
    return t158
}

func main() {
    main0()
}
