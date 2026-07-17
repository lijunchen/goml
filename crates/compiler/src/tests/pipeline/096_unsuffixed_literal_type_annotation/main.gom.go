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
    var retv69 uint8
    retv69 = x__0
    return retv69
}

func take_f32(x__1 float32) float32 {
    var retv71 float32
    retv71 = x__1
    return retv71
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
    var t73 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t73)
    var t74 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t74)
    var t75 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t75)
    var t76 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t76)
    var t77 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t77)
    var t78 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t78)
    var t79 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t79)
    var t80 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t80)
    var t81 uint8 = take_u8(10)
    var t82 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t81)
    println__T_string(t82)
    var t83 float32 = take_f32(2.5)
    var t84 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t83)
    println__T_string(t84)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv89 string
    var t90 string = _goml_runtime_core_uint8_to_string(self__40)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__36 int8) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int8_to_string(self__36)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__37 int16) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int16_to_string(self__37)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__41 uint16) string {
    var retv98 string
    var t99 string = _goml_runtime_core_uint16_to_string(self__41)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__42 uint32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_uint32_to_string(self__42)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__39 int64) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int64_to_string(self__39)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__43 uint64) string {
    var retv107 string
    var t108 string = _goml_runtime_core_uint64_to_string(self__43)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_float32_to_string(self__44)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv113 string
    retv113 = self__34
    return retv113
}

func main() {
    main0()
}
