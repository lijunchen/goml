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
    var t65 int32 = apply__T_int32(1)
    println__T_int32(t65)
    var t66 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(2)
    println__T_int32(t66)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func apply__T_int32(value__2 int32) int32 {
    var retv71 int32
    var t72 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(value__2)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    var retv74 int32
    retv74 = 42
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__41)
    retv76 = t77
    return retv76
}

func main() {
    main0()
}
