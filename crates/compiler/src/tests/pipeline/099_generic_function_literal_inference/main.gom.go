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
    var t62 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t62)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t63 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t63)
    var c__3 int64 = identity__T_int64(100)
    var t64 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t64)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv66 uint8
    retv66 = x__0
    return retv66
}

func println__T_string(value__1 string) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv71 string
    var t72 string = _goml_runtime_core_uint8_to_string(self__40)
    retv71 = t72
    return retv71
}

func identity__T_float32(x__0 float32) float32 {
    var retv74 float32
    retv74 = x__0
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_float32_to_string(self__44)
    retv76 = t77
    return retv76
}

func identity__T_int64(x__0 int64) int64 {
    var retv79 int64
    retv79 = x__0
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__39 int64) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int64_to_string(self__39)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv84 string
    retv84 = self__34
    return retv84
}

func main() {
    main0()
}
