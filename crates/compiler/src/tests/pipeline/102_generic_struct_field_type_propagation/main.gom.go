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
    var t61 uint8 = p__0.first
    var t62 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t61)
    println__T_string(t62)
    var t63 float32 = p__0.second
    var t64 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t63)
    println__T_string(t64)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t66)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv69 string
    var t70 string = _goml_runtime_core_uint8_to_string(self__40)
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv72 string
    var t73 string = _goml_runtime_core_float32_to_string(self__44)
    retv72 = t73
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv75 string
    retv75 = self__34
    return retv75
}

func main() {
    main0()
}
