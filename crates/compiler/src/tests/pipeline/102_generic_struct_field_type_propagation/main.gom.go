package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Pair__uint8__float32 struct {
    first uint8
    second float32
}

func main0() struct{} {
    var p__0 Pair__uint8__float32 = Pair__uint8__float32{
        first: 10,
        second: 3.140000104904175,
    }
    var t25 uint8 = p__0.first
    var t26 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t25)
    println__T_string(t26)
    var t27 float32 = p__0.second
    var t28 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t27)
    println__T_string(t28)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv33 string
    var t34 string = _goml_runtime_core_uint8_to_string(self__15)
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_float32_to_string(self__19)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
