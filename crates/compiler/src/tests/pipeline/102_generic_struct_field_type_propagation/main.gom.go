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
    var t7 uint8 = p__0.first
    var t8 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t7)
    println__T_string(t8)
    var t9 float32 = p__0.second
    var t10 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t9)
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t12 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv15 string
    var t16 string = _goml_runtime_core_uint8_to_string(self__15)
    retv15 = t16
    return retv15
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv18 string
    var t19 string = _goml_runtime_core_float32_to_string(self__19)
    retv18 = t19
    return retv18
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv21 string
    retv21 = self__9
    return retv21
}

func main() {
    main0()
}
