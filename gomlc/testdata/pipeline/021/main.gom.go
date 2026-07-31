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
    var retv160 int32
    var jp162 int32
    switch n__0 {
    case 0:
        jp162 = 10
    case 1:
        jp162 = 20
    default:
        jp162 = 30
    }
    retv160 = jp162
    return retv160
}

func wildcard_first(n__1 int32) int32 {
    var retv164 int32
    retv164 = 40
    return retv164
}

func wildcard_middle(n__2 int32) int32 {
    var retv166 int32
    var jp168 int32
    switch n__2 {
    case 2:
        jp168 = 90
    case 3:
        jp168 = 100
    default:
        jp168 = 100
    }
    retv166 = jp168
    return retv166
}

func repeated(n__3 int32) int32 {
    var retv170 int32
    var jp172 int32
    switch n__3 {
    case 1:
        jp172 = 60
    default:
        jp172 = 80
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t174 int32 = match_int(0)
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t174)
    println__T_string(t175)
    var t176 int32 = match_int(5)
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t176)
    println__T_string(t177)
    var t178 int32 = wildcard_first(0)
    var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t178)
    println__T_string(t179)
    var t180 int32 = wildcard_first(2)
    var t181 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t180)
    println__T_string(t181)
    var t182 int32 = wildcard_middle(2)
    var t183 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t182)
    println__T_string(t183)
    var t184 int32 = wildcard_middle(3)
    var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t184)
    println__T_string(t185)
    var t186 int32 = repeated(1)
    var t187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t186)
    println__T_string(t187)
    var t188 int32 = repeated(3)
    var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t188)
    println__T_string(t189)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
