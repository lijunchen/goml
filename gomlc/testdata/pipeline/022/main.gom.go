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
    var t203 int32
    var inline265 string = "hello"
    switch inline265 {
    case "hello":
        t203 = 1
    case "world":
        t203 = 2
    default:
        t203 = 3
    }
    var t204 string
    var inline263 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline260)
    var t205 int32
    var inline258 string = "planet"
    switch inline258 {
    case "hello":
        t205 = 1
    case "world":
        t205 = 2
    default:
        t205 = 3
    }
    var t206 string
    var inline256 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline253)
    var t207 int32
    t207 = 4
    var t208 string
    var inline249 string = _goml_runtime_core_int32_to_string(t207)
    t208 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline246)
    var t209 int32
    t209 = 4
    var t210 string
    var inline242 string = _goml_runtime_core_int32_to_string(t209)
    t210 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline239)
    var t211 int32
    var inline237 string = "hello"
    switch inline237 {
    case "hello":
        t211 = 6
    default:
        t211 = 8
    }
    var t212 string
    var inline235 string = _goml_runtime_core_int32_to_string(t211)
    t212 = inline235
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline232)
    var t213 int32
    var inline230 string = "mars"
    switch inline230 {
    case "hello":
        t213 = 6
    default:
        t213 = 8
    }
    var t214 string
    var inline228 string = _goml_runtime_core_int32_to_string(t213)
    t214 = inline228
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
