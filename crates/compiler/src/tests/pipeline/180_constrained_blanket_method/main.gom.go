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
    var t62 int32 = apply__T_int32(1)
    println__T_int32(t62)
    var t63 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(2)
    println__T_int32(t63)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func apply__T_int32(value__2 int32) int32 {
    var retv68 int32
    var t69 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(value__2)
    retv68 = t69
    return retv68
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    var retv71 int32
    retv71 = 42
    return retv71
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__38)
    retv73 = t74
    return retv73
}

func main() {
    main0()
}
