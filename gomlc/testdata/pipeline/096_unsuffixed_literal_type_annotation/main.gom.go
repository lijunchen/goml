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
    var t197 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t197)
    var t198 string
    var inline286 string = _goml_runtime_core_int8_to_string(b__3)
    t198 = inline286
    var inline283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline283)
    var t199 string
    var inline281 string = _goml_runtime_core_int16_to_string(c__4)
    t199 = inline281
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline278)
    var t200 string
    var inline276 string = _goml_runtime_core_uint16_to_string(d__5)
    t200 = inline276
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline273)
    var t201 string
    var inline271 string = _goml_runtime_core_uint32_to_string(e__6)
    t201 = inline271
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline268)
    var t202 string
    var inline266 string = _goml_runtime_core_int64_to_string(f__7)
    t202 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline263)
    var t203 string
    var inline261 string = _goml_runtime_core_uint64_to_string(g__8)
    t203 = inline261
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline258)
    var t204 string
    var inline256 string = _goml_runtime_core_float32_to_string(h__9)
    t204 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline253)
    var t205 uint8
    var inline251 uint8 = 10
    t205 = inline251
    var t206 string
    var inline249 string = _goml_runtime_core_uint8_to_string(t205)
    t206 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline246)
    var t207 float32
    var inline244 float32 = 2.5
    t207 = inline244
    var t208 string
    var inline242 string = _goml_runtime_core_float32_to_string(t207)
    t208 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline239)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t210 string
    t210 = value__1
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t214 string = _goml_runtime_core_uint8_to_string(self__72)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
