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

func main0() struct{} {
    var first__3 int32 = 1
    var second__4 int32 = 2
    var t159 int32 = apply__T_int32(first__3)
    println__T_int32(t159)
    var t160 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(second__4)
    println__T_int32(t160)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func apply__T_int32(value__2 int32) int32 {
    var t166 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(value__2)
    return t166
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    return t171
}

func main() {
    main0()
}
