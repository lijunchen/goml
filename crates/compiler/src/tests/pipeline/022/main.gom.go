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
    var retv70 int32
    var jp72 int32
    switch s__0 {
    case "hello":
        jp72 = 1
    case "world":
        jp72 = 2
    default:
        jp72 = 3
    }
    retv70 = jp72
    return retv70
}

func wildcard_position(s__1 string) int32 {
    var retv74 int32
    retv74 = 4
    return retv74
}

func repeated_string(s__2 string) int32 {
    var retv76 int32
    var jp78 int32
    switch s__2 {
    case "hello":
        jp78 = 6
    default:
        jp78 = 8
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t80 int32 = match_string("hello")
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    println__T_string(t81)
    var t82 int32 = match_string("planet")
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    var t84 int32 = wildcard_position("world")
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 int32 = wildcard_position("sun")
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    println__T_string(t87)
    var t88 int32 = repeated_string("hello")
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    println__T_string(t89)
    var t90 int32 = repeated_string("mars")
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    println__T_string(t91)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__6)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func main() {
    main0()
}
