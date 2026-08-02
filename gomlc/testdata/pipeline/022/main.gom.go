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
    switch s__0 {
    case "hello":
        return 1
    case "world":
        return 2
    default:
        return 3
    }
}

func wildcard_position(s__1 string) int32 {
    return 4
}

func repeated_string(s__2 string) int32 {
    switch s__2 {
    case "hello":
        return 6
    default:
        return 8
    }
}

func main0() struct{} {
    var t171 int32 = match_string("hello")
    var t172 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t171)
    println__T_string(t172)
    var t173 int32 = match_string("planet")
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t173)
    println__T_string(t174)
    var t175 int32 = wildcard_position("world")
    var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t175)
    println__T_string(t176)
    var t177 int32 = wildcard_position("sun")
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t177)
    println__T_string(t178)
    var t179 int32 = repeated_string("hello")
    var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t179)
    println__T_string(t180)
    var t181 int32 = repeated_string("mars")
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t181)
    println__T_string(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t189 string = _goml_runtime_core_int32_to_string(self__6)
    return t189
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
