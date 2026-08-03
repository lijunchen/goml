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

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t151 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t151)
    var t152 string
    var inline240 string = _goml_runtime_core_int8_to_string(b__3)
    t152 = inline240
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline237)
    var t153 string
    var inline235 string = _goml_runtime_core_int16_to_string(c__4)
    t153 = inline235
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline232)
    var t154 string
    var inline230 string = _goml_runtime_core_uint16_to_string(d__5)
    t154 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline227)
    var t155 string
    var inline225 string = _goml_runtime_core_uint32_to_string(e__6)
    t155 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline222)
    var t156 string
    var inline220 string = _goml_runtime_core_int64_to_string(f__7)
    t156 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
    _goml_runtime_core_string_println(inline217)
    var t157 string
    var inline215 string = _goml_runtime_core_uint64_to_string(g__8)
    t157 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline212)
    var t158 string
    var inline210 string = _goml_runtime_core_float32_to_string(h__9)
    t158 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline207)
    var t159 uint8
    var inline205 uint8 = 10
    t159 = inline205
    var t160 string
    var inline203 string = _goml_runtime_core_uint8_to_string(t159)
    t160 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline200)
    var t161 float32
    var inline198 float32 = 2.5
    t161 = inline198
    var t162 string
    var inline196 string = _goml_runtime_core_float32_to_string(t161)
    t162 = inline196
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
    _goml_runtime_core_string_println(inline193)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t164 string
    t164 = value__31
    _goml_runtime_core_string_println(t164)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t168 string = _goml_runtime_core_uint8_to_string(self__74)
    return t168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
