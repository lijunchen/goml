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
    var t170 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t170)
    var t171 string
    var inline259 string = _goml_runtime_core_int8_to_string(b__3)
    t171 = inline259
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline256)
    var t172 string
    var inline254 string = _goml_runtime_core_int16_to_string(c__4)
    t172 = inline254
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline251)
    var t173 string
    var inline249 string = _goml_runtime_core_uint16_to_string(d__5)
    t173 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline246)
    var t174 string
    var inline244 string = _goml_runtime_core_uint32_to_string(e__6)
    t174 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline241)
    var t175 string
    var inline239 string = _goml_runtime_core_int64_to_string(f__7)
    t175 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline236)
    var t176 string
    var inline234 string = _goml_runtime_core_uint64_to_string(g__8)
    t176 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline231)
    var t177 string
    var inline229 string = _goml_runtime_core_float32_to_string(h__9)
    t177 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline226)
    var t178 uint8
    var inline224 uint8 = 10
    t178 = inline224
    var t179 string
    var inline222 string = _goml_runtime_core_uint8_to_string(t178)
    t179 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline219)
    var t180 float32
    var inline217 float32 = 2.5
    t180 = inline217
    var t181 string
    var inline215 string = _goml_runtime_core_float32_to_string(t180)
    t181 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t183 string
    t183 = value__1
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t187 string = _goml_runtime_core_uint8_to_string(self__45)
    return t187
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
