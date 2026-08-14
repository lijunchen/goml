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
    var inline444 struct{} = struct{}{}
    var inline445 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline444)
    _goml_runtime_core_string_print(inline445)
    var inline440 bool = true
    var inline441 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline440)
    _goml_runtime_core_string_print(inline441)
    var inline436 bool = false
    var inline437 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline436)
    _goml_runtime_core_string_print(inline437)
    var inline432 int = 123
    var inline433 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline432)
    _goml_runtime_core_string_print(inline433)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t424 string = _goml_runtime_core_unit_to_string(self__147)
    return t424
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t427 string = _goml_runtime_core_bool_to_string(self__148)
    return t427
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t430 string = _goml_runtime_core_int_to_string(self__151)
    return t430
}

func main() {
    main0()
}
