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
    var t10 uint8 = p__0.first
    var t11 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t10)
    println__T_string(t11)
    var t12 float32 = p__0.second
    var t13 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t12)
    println__T_string(t13)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t15 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t15)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv18 string
    var t19 string = _goml_runtime_core_uint8_to_string(self__15)
    retv18 = t19
    return retv18
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv21 string
    var t22 string = _goml_runtime_core_float32_to_string(self__19)
    retv21 = t22
    return retv21
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv24 string
    retv24 = self__9
    return retv24
}

func main() {
    main0()
}
