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
    var t26 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t26)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t27 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t27)
    var c__3 int64 = identity__T_int64(100)
    var t28 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t28)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv30 uint8
    retv30 = x__0
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv35 string
    var t36 string = _goml_runtime_core_uint8_to_string(self__15)
    retv35 = t36
    return retv35
}

func identity__T_float32(x__0 float32) float32 {
    var retv38 float32
    retv38 = x__0
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_float32_to_string(self__19)
    retv40 = t41
    return retv40
}

func identity__T_int64(x__0 int64) int64 {
    var retv43 int64
    retv43 = x__0
    return retv43
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv45 string
    var t46 string = _goml_runtime_core_int64_to_string(self__14)
    retv45 = t46
    return retv45
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
