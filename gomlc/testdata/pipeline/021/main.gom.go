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

func match_int(n__0 int32) int32 {
    switch n__0 {
    case 0:
        return 10
    case 1:
        return 20
    default:
        return 30
    }
}

func main0() struct{} {
    var t199 int32 = match_int(0)
    var t200 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t199)
    println__T_string(t200)
    var t201 int32 = match_int(5)
    var t202 string
    var inline267 string = _goml_runtime_core_int32_to_string(t201)
    t202 = inline267
    println__T_string(t202)
    var t203 int32
    t203 = 40
    var t204 string
    var inline263 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline260)
    var t205 int32
    t205 = 40
    var t206 string
    var inline256 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline253)
    var t207 int32
    var inline251 int32 = 2
    switch inline251 {
    case 2:
        t207 = 90
    case 3:
        t207 = 100
    default:
        t207 = 100
    }
    var t208 string
    var inline249 string = _goml_runtime_core_int32_to_string(t207)
    t208 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline246)
    var t209 int32
    var inline244 int32 = 3
    switch inline244 {
    case 2:
        t209 = 90
    case 3:
        t209 = 100
    default:
        t209 = 100
    }
    var t210 string
    var inline242 string = _goml_runtime_core_int32_to_string(t209)
    t210 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline239)
    var t211 int32
    var inline237 int32 = 1
    switch inline237 {
    case 1:
        t211 = 60
    default:
        t211 = 80
    }
    var t212 string
    var inline235 string = _goml_runtime_core_int32_to_string(t211)
    t212 = inline235
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline232)
    var t213 int32
    var inline230 int32 = 3
    switch inline230 {
    case 1:
        t213 = 60
    default:
        t213 = 80
    }
    var t214 string
    var inline228 string = _goml_runtime_core_int32_to_string(t213)
    t214 = inline228
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t217 string
    t217 = value__31
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__35)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
