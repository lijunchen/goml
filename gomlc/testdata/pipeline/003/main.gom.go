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
    var inline191 struct{} = struct{}{}
    var inline192 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline191)
    _goml_runtime_core_string_print(inline192)
    var inline187 bool = true
    var inline188 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline187)
    _goml_runtime_core_string_print(inline188)
    var inline183 bool = false
    var inline184 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline183)
    _goml_runtime_core_string_print(inline184)
    var inline179 int = 123
    var inline180 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline179)
    _goml_runtime_core_string_print(inline180)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var t171 string = _goml_runtime_core_unit_to_string(self__36)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t174 string = _goml_runtime_core_bool_to_string(self__37)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t177 string = _goml_runtime_core_int_to_string(self__40)
    return t177
}

func main() {
    main0()
}
