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
    var retv119 uint8
    retv119 = x__0
    return retv119
}

func take_f32(x__1 float32) float32 {
    var retv121 float32
    retv121 = x__1
    return retv121
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
    var t123 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t123)
    var t124 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t124)
    var t125 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t125)
    var t126 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t126)
    var t127 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t127)
    var t128 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t128)
    var t129 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t129)
    var t130 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t130)
    var t131 uint8 = take_u8(10)
    var t132 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t131)
    println__T_string(t132)
    var t133 float32 = take_f32(2.5)
    var t134 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t133)
    println__T_string(t134)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t136 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t136)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv139 string
    var t140 string = _goml_runtime_core_uint8_to_string(self__45)
    retv139 = t140
    return retv139
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int8_to_string(self__41)
    retv142 = t143
    return retv142
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv145 string
    var t146 string = _goml_runtime_core_int16_to_string(self__42)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv148 string
    var t149 string = _goml_runtime_core_uint16_to_string(self__46)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_uint32_to_string(self__47)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv154 string
    var t155 string = _goml_runtime_core_int64_to_string(self__44)
    retv154 = t155
    return retv154
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv157 string
    var t158 string = _goml_runtime_core_uint64_to_string(self__48)
    retv157 = t158
    return retv157
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv160 string
    var t161 string = _goml_runtime_core_float32_to_string(self__49)
    retv160 = t161
    return retv160
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv163 string
    retv163 = self__38
    return retv163
}

func main() {
    main0()
}
