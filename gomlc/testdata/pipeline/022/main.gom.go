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
    var retv114 int32
    var jp116 int32
    switch s__0 {
    case "hello":
        jp116 = 1
    case "world":
        jp116 = 2
    default:
        jp116 = 3
    }
    retv114 = jp116
    return retv114
}

func wildcard_position(s__1 string) int32 {
    var retv118 int32
    retv118 = 4
    return retv118
}

func repeated_string(s__2 string) int32 {
    var retv120 int32
    var jp122 int32
    switch s__2 {
    case "hello":
        jp122 = 6
    default:
        jp122 = 8
    }
    retv120 = jp122
    return retv120
}

func main0() struct{} {
    var t124 int32 = match_string("hello")
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t124)
    println__T_string(t125)
    var t126 int32 = match_string("planet")
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    var t128 int32 = wildcard_position("world")
    var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t128)
    println__T_string(t129)
    var t130 int32 = wildcard_position("sun")
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    println__T_string(t131)
    var t132 int32 = repeated_string("hello")
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    println__T_string(t133)
    var t134 int32 = repeated_string("mars")
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t134)
    println__T_string(t135)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv141 string
    var t142 string = _goml_runtime_core_int32_to_string(self__6)
    retv141 = t142
    return retv141
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv144 string
    retv144 = self__38
    return retv144
}

func main() {
    main0()
}
