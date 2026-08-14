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
    var t198 int32
    var inline260 string = "hello"
    switch inline260 {
    case "hello":
        t198 = 1
    case "world":
        t198 = 2
    default:
        t198 = 3
    }
    var t199 string
    var inline258 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline255)
    var t200 int32
    var inline253 string = "planet"
    switch inline253 {
    case "hello":
        t200 = 1
    case "world":
        t200 = 2
    default:
        t200 = 3
    }
    var t201 string
    var inline251 string = _goml_runtime_core_int32_to_string(t200)
    t201 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline248)
    var t202 int32
    t202 = 4
    var t203 string
    var inline244 string = _goml_runtime_core_int32_to_string(t202)
    t203 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline241)
    var t204 int32
    t204 = 4
    var t205 string
    var inline237 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline237
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline234)
    var t206 int32
    var inline232 string = "hello"
    switch inline232 {
    case "hello":
        t206 = 6
    default:
        t206 = 8
    }
    var t207 string
    var inline230 string = _goml_runtime_core_int32_to_string(t206)
    t207 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline227)
    var t208 int32
    var inline225 string = "mars"
    switch inline225 {
    case "hello":
        t208 = 6
    default:
        t208 = 8
    }
    var t209 string
    var inline223 string = _goml_runtime_core_int32_to_string(t208)
    t209 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
