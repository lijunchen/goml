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
    var t204 int32 = match_int(0)
    var t205 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t204)
    println__T_string(t205)
    var t206 int32 = match_int(5)
    var t207 string
    var inline272 string = _goml_runtime_core_int32_to_string(t206)
    t207 = inline272
    println__T_string(t207)
    var t208 int32
    t208 = 40
    var t209 string
    var inline268 string = _goml_runtime_core_int32_to_string(t208)
    t209 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline265)
    var t210 int32
    t210 = 40
    var t211 string
    var inline261 string = _goml_runtime_core_int32_to_string(t210)
    t211 = inline261
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline258)
    var t212 int32
    var inline256 int32 = 2
    switch inline256 {
    case 2:
        t212 = 90
    case 3:
        t212 = 100
    default:
        t212 = 100
    }
    var t213 string
    var inline254 string = _goml_runtime_core_int32_to_string(t212)
    t213 = inline254
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline251)
    var t214 int32
    var inline249 int32 = 3
    switch inline249 {
    case 2:
        t214 = 90
    case 3:
        t214 = 100
    default:
        t214 = 100
    }
    var t215 string
    var inline247 string = _goml_runtime_core_int32_to_string(t214)
    t215 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline244)
    var t216 int32
    var inline242 int32 = 1
    switch inline242 {
    case 1:
        t216 = 60
    default:
        t216 = 80
    }
    var t217 string
    var inline240 string = _goml_runtime_core_int32_to_string(t216)
    t217 = inline240
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline237)
    var t218 int32
    var inline235 int32 = 3
    switch inline235 {
    case 1:
        t218 = 60
    default:
        t218 = 80
    }
    var t219 string
    var inline233 string = _goml_runtime_core_int32_to_string(t218)
    t219 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t222 string
    t222 = value__1
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__33)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
