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

type Ordering int32

func main0() struct{} {
    var inline447 struct{} = struct{}{}
    var inline448 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline447)
    _goml_runtime_core_string_print(inline448)
    var inline443 bool = true
    var inline444 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline443)
    _goml_runtime_core_string_print(inline444)
    var inline439 bool = false
    var inline440 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline439)
    _goml_runtime_core_string_print(inline440)
    var inline435 int = 123
    var inline436 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline435)
    _goml_runtime_core_string_print(inline436)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t427 string = _goml_runtime_core_unit_to_string(self__147)
    return t427
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t430 string = _goml_runtime_core_bool_to_string(self__148)
    return t430
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t433 string = _goml_runtime_core_int_to_string(self__151)
    return t433
}

func main() {
    main0()
}
