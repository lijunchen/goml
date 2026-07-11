package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func main0() struct{} {
    var a__1 uint8 = identity__T_uint8(42)
    var t8 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t8)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t9 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t9)
    var c__3 int64 = identity__T_int64(100)
    var t10 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t10)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv12 uint8
    retv12 = x__0
    return retv12
}

func println__T_string(value__1 string) struct{} {
    var t14 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t14)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv17 string
    var t18 string = _goml_runtime_core_uint8_to_string(self__15)
    retv17 = t18
    return retv17
}

func identity__T_float32(x__0 float32) float32 {
    var retv20 float32
    retv20 = x__0
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv22 string
    var t23 string = _goml_runtime_core_float32_to_string(self__19)
    retv22 = t23
    return retv22
}

func identity__T_int64(x__0 int64) int64 {
    var retv25 int64
    retv25 = x__0
    return retv25
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv27 string
    var t28 string = _goml_runtime_core_int64_to_string(self__14)
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv30 string
    retv30 = self__9
    return retv30
}

func main() {
    main0()
}
