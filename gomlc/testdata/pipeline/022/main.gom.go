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

func match_string(s__0 string) int32 {
    var retv158 int32
    var jp160 int32
    switch s__0 {
    case "hello":
        jp160 = 1
    case "world":
        jp160 = 2
    default:
        jp160 = 3
    }
    retv158 = jp160
    return retv158
}

func wildcard_position(s__1 string) int32 {
    var retv162 int32
    retv162 = 4
    return retv162
}

func repeated_string(s__2 string) int32 {
    var retv164 int32
    var jp166 int32
    switch s__2 {
    case "hello":
        jp166 = 6
    default:
        jp166 = 8
    }
    retv164 = jp166
    return retv164
}

func main0() struct{} {
    var t168 int32 = match_string("hello")
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t168)
    println__T_string(t169)
    var t170 int32 = match_string("planet")
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t170)
    println__T_string(t171)
    var t172 int32 = wildcard_position("world")
    var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t172)
    println__T_string(t173)
    var t174 int32 = wildcard_position("sun")
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t174)
    println__T_string(t175)
    var t176 int32 = repeated_string("hello")
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t176)
    println__T_string(t177)
    var t178 int32 = repeated_string("mars")
    var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t178)
    println__T_string(t179)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv185 string
    var t186 string = _goml_runtime_core_int32_to_string(self__6)
    retv185 = t186
    return retv185
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv188 string
    retv188 = self__38
    return retv188
}

func main() {
    main0()
}
