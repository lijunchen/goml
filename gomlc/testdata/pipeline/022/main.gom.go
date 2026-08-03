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
    var t193 int32
    var inline255 string = "hello"
    switch inline255 {
    case "hello":
        t193 = 1
    case "world":
        t193 = 2
    default:
        t193 = 3
    }
    var t194 string
    var inline253 string = _goml_runtime_core_int32_to_string(t193)
    t194 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline250)
    var t195 int32
    var inline248 string = "planet"
    switch inline248 {
    case "hello":
        t195 = 1
    case "world":
        t195 = 2
    default:
        t195 = 3
    }
    var t196 string
    var inline246 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline243)
    var t197 int32
    t197 = 4
    var t198 string
    var inline239 string = _goml_runtime_core_int32_to_string(t197)
    t198 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline236)
    var t199 int32
    t199 = 4
    var t200 string
    var inline232 string = _goml_runtime_core_int32_to_string(t199)
    t200 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline229)
    var t201 int32
    var inline227 string = "hello"
    switch inline227 {
    case "hello":
        t201 = 6
    default:
        t201 = 8
    }
    var t202 string
    var inline225 string = _goml_runtime_core_int32_to_string(t201)
    t202 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline222)
    var t203 int32
    var inline220 string = "mars"
    switch inline220 {
    case "hello":
        t203 = 6
    default:
        t203 = 8
    }
    var t204 string
    var inline218 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
