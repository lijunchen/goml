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
    var retv163 int32
    var jp165 int32
    switch n__0 {
    case 0:
        jp165 = 10
    case 1:
        jp165 = 20
    default:
        jp165 = 30
    }
    retv163 = jp165
    return retv163
}

func wildcard_first(n__1 int32) int32 {
    var retv167 int32
    retv167 = 40
    return retv167
}

func wildcard_middle(n__2 int32) int32 {
    var retv169 int32
    var jp171 int32
    switch n__2 {
    case 2:
        jp171 = 90
    case 3:
        jp171 = 100
    default:
        jp171 = 100
    }
    retv169 = jp171
    return retv169
}

func repeated(n__3 int32) int32 {
    var retv173 int32
    var jp175 int32
    switch n__3 {
    case 1:
        jp175 = 60
    default:
        jp175 = 80
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var t177 int32 = match_int(0)
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t177)
    println__T_string(t178)
    var t179 int32 = match_int(5)
    var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t179)
    println__T_string(t180)
    var t181 int32 = wildcard_first(0)
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t181)
    println__T_string(t182)
    var t183 int32 = wildcard_first(2)
    var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t183)
    println__T_string(t184)
    var t185 int32 = wildcard_middle(2)
    var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t185)
    println__T_string(t186)
    var t187 int32 = wildcard_middle(3)
    var t188 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t187)
    println__T_string(t188)
    var t189 int32 = repeated(1)
    var t190 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t189)
    println__T_string(t190)
    var t191 int32 = repeated(3)
    var t192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t191)
    println__T_string(t192)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv198 string
    var t199 string = _goml_runtime_core_int32_to_string(self__6)
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv201 string
    retv201 = self__38
    return retv201
}

func main() {
    main0()
}
