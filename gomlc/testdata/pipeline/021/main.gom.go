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
    var t194 int32 = match_int(0)
    var t195 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t194)
    println__T_string(t195)
    var t196 int32 = match_int(5)
    var t197 string
    var inline262 string = _goml_runtime_core_int32_to_string(t196)
    t197 = inline262
    println__T_string(t197)
    var t198 int32
    t198 = 40
    var t199 string
    var inline258 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline255)
    var t200 int32
    t200 = 40
    var t201 string
    var inline251 string = _goml_runtime_core_int32_to_string(t200)
    t201 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline248)
    var t202 int32
    var inline246 int32 = 2
    switch inline246 {
    case 2:
        t202 = 90
    case 3:
        t202 = 100
    default:
        t202 = 100
    }
    var t203 string
    var inline244 string = _goml_runtime_core_int32_to_string(t202)
    t203 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline241)
    var t204 int32
    var inline239 int32 = 3
    switch inline239 {
    case 2:
        t204 = 90
    case 3:
        t204 = 100
    default:
        t204 = 100
    }
    var t205 string
    var inline237 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline237
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline234)
    var t206 int32
    var inline232 int32 = 1
    switch inline232 {
    case 1:
        t206 = 60
    default:
        t206 = 80
    }
    var t207 string
    var inline230 string = _goml_runtime_core_int32_to_string(t206)
    t207 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline227)
    var t208 int32
    var inline225 int32 = 3
    switch inline225 {
    case 1:
        t208 = 60
    default:
        t208 = 80
    }
    var t209 string
    var inline223 string = _goml_runtime_core_int32_to_string(t208)
    t209 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t212 string
    t212 = value__31
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t216 string = _goml_runtime_core_int32_to_string(self__35)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
