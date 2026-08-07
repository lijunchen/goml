package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t188 int32
    var inline250 string = "hello"
    switch inline250 {
    case "hello":
        t188 = 1
    case "world":
        t188 = 2
    default:
        t188 = 3
    }
    var t189 string
    var inline248 string = _goml_runtime_core_int32_to_string(t188)
    t189 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline245)
    var t190 int32
    var inline243 string = "planet"
    switch inline243 {
    case "hello":
        t190 = 1
    case "world":
        t190 = 2
    default:
        t190 = 3
    }
    var t191 string
    var inline241 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline238)
    var t192 int32
    t192 = 4
    var t193 string
    var inline234 string = _goml_runtime_core_int32_to_string(t192)
    t193 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline231)
    var t194 int32
    t194 = 4
    var t195 string
    var inline227 string = _goml_runtime_core_int32_to_string(t194)
    t195 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline224)
    var t196 int32
    var inline222 string = "hello"
    switch inline222 {
    case "hello":
        t196 = 6
    default:
        t196 = 8
    }
    var t197 string
    var inline220 string = _goml_runtime_core_int32_to_string(t196)
    t197 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline217)
    var t198 int32
    var inline215 string = "mars"
    switch inline215 {
    case "hello":
        t198 = 6
    default:
        t198 = 8
    }
    var t199 string
    var inline213 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
