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
    var t187 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t187)
    var t188 string
    var inline276 string = _goml_runtime_core_int8_to_string(b__3)
    t188 = inline276
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline273)
    var t189 string
    var inline271 string = _goml_runtime_core_int16_to_string(c__4)
    t189 = inline271
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline268)
    var t190 string
    var inline266 string = _goml_runtime_core_uint16_to_string(d__5)
    t190 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline263)
    var t191 string
    var inline261 string = _goml_runtime_core_uint32_to_string(e__6)
    t191 = inline261
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline258)
    var t192 string
    var inline256 string = _goml_runtime_core_int64_to_string(f__7)
    t192 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline253)
    var t193 string
    var inline251 string = _goml_runtime_core_uint64_to_string(g__8)
    t193 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline248)
    var t194 string
    var inline246 string = _goml_runtime_core_float32_to_string(h__9)
    t194 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline243)
    var t195 uint8
    var inline241 uint8 = 10
    t195 = inline241
    var t196 string
    var inline239 string = _goml_runtime_core_uint8_to_string(t195)
    t196 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline236)
    var t197 float32
    var inline234 float32 = 2.5
    t197 = inline234
    var t198 string
    var inline232 string = _goml_runtime_core_float32_to_string(t197)
    t198 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline229)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t200 string
    t200 = value__31
    _goml_runtime_core_string_println(t200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t204 string = _goml_runtime_core_uint8_to_string(self__74)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
