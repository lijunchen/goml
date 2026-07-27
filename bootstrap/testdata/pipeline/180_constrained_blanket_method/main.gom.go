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
    var t68 int32 = apply__T_int32(first__3)
    println__T_int32(t68)
    var t69 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(second__4)
    println__T_int32(t69)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func apply__T_int32(value__2 int32) int32 {
    var retv74 int32
    var t75 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(value__2)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    var retv77 int32
    retv77 = 42
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__43)
    retv79 = t80
    return retv79
}

func main() {
    main0()
}
