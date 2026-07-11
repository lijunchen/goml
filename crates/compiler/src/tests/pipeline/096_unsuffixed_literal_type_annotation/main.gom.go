package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func take_u8(x__0 uint8) uint8 {
    var retv15 uint8
    retv15 = x__0
    return retv15
}

func take_f32(x__1 float32) float32 {
    var retv17 float32
    retv17 = x__1
    return retv17
}

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t19 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t19)
    var t20 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t20)
    var t21 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t21)
    var t22 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t22)
    var t23 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t23)
    var t24 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t24)
    var t25 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t25)
    var t26 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t26)
    var t27 uint8 = take_u8(10)
    var t28 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t27)
    println__T_string(t28)
    var t29 float32 = take_f32(2.5)
    var t30 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t29)
    println__T_string(t30)
    return struct{}{}
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

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int8_to_string(self__11)
    retv38 = t39
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv41 string
    var t42 string = _goml_runtime_core_int16_to_string(self__12)
    retv41 = t42
    return retv41
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__16 uint16) string {
    var retv44 string
    var t45 string = _goml_runtime_core_uint16_to_string(self__16)
    retv44 = t45
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__17 uint32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_uint32_to_string(self__17)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv50 string
    var t51 string = _goml_runtime_core_int64_to_string(self__14)
    retv50 = t51
    return retv50
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__18 uint64) string {
    var retv53 string
    var t54 string = _goml_runtime_core_uint64_to_string(self__18)
    retv53 = t54
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_float32_to_string(self__19)
    retv56 = t57
    return retv56
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv59 string
    retv59 = self__9
    return retv59
}

func main() {
    main0()
}
