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
    var retv74 int32
    var jp76 int32
    switch s__0 {
    case "hello":
        jp76 = 1
    case "world":
        jp76 = 2
    default:
        jp76 = 3
    }
    retv74 = jp76
    return retv74
}

func wildcard_position(s__1 string) int32 {
    var retv78 int32
    retv78 = 4
    return retv78
}

func repeated_string(s__2 string) int32 {
    var retv80 int32
    var jp82 int32
    switch s__2 {
    case "hello":
        jp82 = 6
    default:
        jp82 = 8
    }
    retv80 = jp82
    return retv80
}

func main0() struct{} {
    var t84 int32 = match_string("hello")
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 int32 = match_string("planet")
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    println__T_string(t87)
    var t88 int32 = wildcard_position("world")
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    println__T_string(t89)
    var t90 int32 = wildcard_position("sun")
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    println__T_string(t91)
    var t92 int32 = repeated_string("hello")
    var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t92)
    println__T_string(t93)
    var t94 int32 = repeated_string("mars")
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t94)
    println__T_string(t95)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__6)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv104 string
    retv104 = self__38
    return retv104
}

func main() {
    main0()
}
