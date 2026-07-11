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
    var retv33 uint8
    retv33 = x__0
    return retv33
}

func take_f32(x__1 float32) float32 {
    var retv35 float32
    retv35 = x__1
    return retv35
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
    var t37 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t37)
    var t38 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t38)
    var t39 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t39)
    var t40 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t40)
    var t41 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t41)
    var t42 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t42)
    var t43 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t43)
    var t44 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t44)
    var t45 uint8 = take_u8(10)
    var t46 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t45)
    println__T_string(t46)
    var t47 float32 = take_f32(2.5)
    var t48 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t47)
    println__T_string(t48)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv53 string
    var t54 string = _goml_runtime_core_uint8_to_string(self__15)
    retv53 = t54
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int8_to_string(self__11)
    retv56 = t57
    return retv56
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv59 string
    var t60 string = _goml_runtime_core_int16_to_string(self__12)
    retv59 = t60
    return retv59
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__16 uint16) string {
    var retv62 string
    var t63 string = _goml_runtime_core_uint16_to_string(self__16)
    retv62 = t63
    return retv62
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__17 uint32) string {
    var retv65 string
    var t66 string = _goml_runtime_core_uint32_to_string(self__17)
    retv65 = t66
    return retv65
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv68 string
    var t69 string = _goml_runtime_core_int64_to_string(self__14)
    retv68 = t69
    return retv68
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__18 uint64) string {
    var retv71 string
    var t72 string = _goml_runtime_core_uint64_to_string(self__18)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_float32_to_string(self__19)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv77 string
    retv77 = self__9
    return retv77
}

func main() {
    main0()
}
