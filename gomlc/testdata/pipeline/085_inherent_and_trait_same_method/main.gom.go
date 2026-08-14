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
    var t418 string
    t418 = "inherent"
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline438)
    var t420 string
    var inline435 int32 = 9
    var inline436 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline435)
    t420 = inline436
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t423 string = _goml_runtime_core_int32_to_string(self__33)
    return t423
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
