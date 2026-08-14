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
    var t209 int32 = match_int(0)
    var t210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t209)
    println__T_string(t210)
    var t211 int32 = match_int(5)
    var t212 string
    var inline277 string = _goml_runtime_core_int32_to_string(t211)
    t212 = inline277
    println__T_string(t212)
    var t213 int32
    t213 = 40
    var t214 string
    var inline273 string = _goml_runtime_core_int32_to_string(t213)
    t214 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline270)
    var t215 int32
    t215 = 40
    var t216 string
    var inline266 string = _goml_runtime_core_int32_to_string(t215)
    t216 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline263)
    var t217 int32
    var inline261 int32 = 2
    switch inline261 {
    case 2:
        t217 = 90
    case 3:
        t217 = 100
    default:
        t217 = 100
    }
    var t218 string
    var inline259 string = _goml_runtime_core_int32_to_string(t217)
    t218 = inline259
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline256)
    var t219 int32
    var inline254 int32 = 3
    switch inline254 {
    case 2:
        t219 = 90
    case 3:
        t219 = 100
    default:
        t219 = 100
    }
    var t220 string
    var inline252 string = _goml_runtime_core_int32_to_string(t219)
    t220 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline249)
    var t221 int32
    var inline247 int32 = 1
    switch inline247 {
    case 1:
        t221 = 60
    default:
        t221 = 80
    }
    var t222 string
    var inline245 string = _goml_runtime_core_int32_to_string(t221)
    t222 = inline245
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline242)
    var t223 int32
    var inline240 int32 = 3
    switch inline240 {
    case 1:
        t223 = 60
    default:
        t223 = 80
    }
    var t224 string
    var inline238 string = _goml_runtime_core_int32_to_string(t223)
    t224 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t227 string
    t227 = value__1
    _goml_runtime_core_string_println(t227)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t231 string = _goml_runtime_core_int32_to_string(self__33)
    return t231
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
