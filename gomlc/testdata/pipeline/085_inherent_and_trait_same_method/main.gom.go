package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t421 string
    t421 = "inherent"
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline441)
    var t423 string
    var inline438 int32 = 9
    var inline439 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline438)
    t423 = inline439
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline435)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t426 string = _goml_runtime_core_int32_to_string(self__33)
    return t426
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
