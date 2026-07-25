package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
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
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func take_u8(x__0 uint8) uint8 {
    var retv75 uint8
    retv75 = x__0
    return retv75
}

func take_f32(x__1 float32) float32 {
    var retv77 float32
    retv77 = x__1
    return retv77
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
    var t79 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t79)
    var t80 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t80)
    var t81 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t81)
    var t82 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t82)
    var t83 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t83)
    var t84 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t84)
    var t85 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t85)
    var t86 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t86)
    var t87 uint8 = take_u8(10)
    var t88 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t87)
    println__T_string(t88)
    var t89 float32 = take_f32(2.5)
    var t90 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t89)
    println__T_string(t90)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv95 string
    var t96 string = _goml_runtime_core_uint8_to_string(self__45)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int8_to_string(self__41)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int16_to_string(self__42)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv104 string
    var t105 string = _goml_runtime_core_uint16_to_string(self__46)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_uint32_to_string(self__47)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int64_to_string(self__44)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv113 string
    var t114 string = _goml_runtime_core_uint64_to_string(self__48)
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_float32_to_string(self__49)
    retv116 = t117
    return retv116
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv119 string
    retv119 = self__38
    return retv119
}

func main() {
    main0()
}
