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
    var t26 int32 = apply__T_int32(1)
    println__T_int32(t26)
    var t27 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(2)
    println__T_int32(t27)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t29 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t29)
    return struct{}{}
}

func apply__T_int32(value__2 int32) int32 {
    var retv32 int32
    var t33 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(value__2)
    retv32 = t33
    return retv32
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    var retv35 int32
    retv35 = 42
    return retv35
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__13)
    retv37 = t38
    return retv37
}

func main() {
    main0()
}
