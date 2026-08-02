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
    var t177 int32 = match_int(0)
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t177)
    println__T_string(t178)
    var t179 int32 = match_int(5)
    var t180 string
    var inline245 string = _goml_runtime_core_int32_to_string(t179)
    t180 = inline245
    println__T_string(t180)
    var t181 int32
    t181 = 40
    var t182 string
    var inline241 string = _goml_runtime_core_int32_to_string(t181)
    t182 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline238)
    var t183 int32
    t183 = 40
    var t184 string
    var inline234 string = _goml_runtime_core_int32_to_string(t183)
    t184 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline231)
    var t185 int32
    var inline229 int32 = 2
    switch inline229 {
    case 2:
        t185 = 90
    case 3:
        t185 = 100
    default:
        t185 = 100
    }
    var t186 string
    var inline227 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline224)
    var t187 int32
    var inline222 int32 = 3
    switch inline222 {
    case 2:
        t187 = 90
    case 3:
        t187 = 100
    default:
        t187 = 100
    }
    var t188 string
    var inline220 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline217)
    var t189 int32
    var inline215 int32 = 1
    switch inline215 {
    case 1:
        t189 = 60
    default:
        t189 = 80
    }
    var t190 string
    var inline213 string = _goml_runtime_core_int32_to_string(t189)
    t190 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline210)
    var t191 int32
    var inline208 int32 = 3
    switch inline208 {
    case 1:
        t191 = 60
    default:
        t191 = 80
    }
    var t192 string
    var inline206 string = _goml_runtime_core_int32_to_string(t191)
    t192 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string
    t195 = value__1
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t199 string = _goml_runtime_core_int32_to_string(self__6)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
