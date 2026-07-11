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
    var t11 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t11)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t12 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t12)
    var c__3 int64 = identity__T_int64(100)
    var t13 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t13)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv15 uint8
    retv15 = x__0
    return retv15
}

func println__T_string(value__1 string) struct{} {
    var t17 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t17)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv20 string
    var t21 string = _goml_runtime_core_uint8_to_string(self__15)
    retv20 = t21
    return retv20
}

func identity__T_float32(x__0 float32) float32 {
    var retv23 float32
    retv23 = x__0
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv25 string
    var t26 string = _goml_runtime_core_float32_to_string(self__19)
    retv25 = t26
    return retv25
}

func identity__T_int64(x__0 int64) int64 {
    var retv28 int64
    retv28 = x__0
    return retv28
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int64_to_string(self__14)
    retv30 = t31
    return retv30
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv33 string
    retv33 = self__9
    return retv33
}

func main() {
    main0()
}
