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
    var retv67 int32
    var jp69 int32
    switch s__0 {
    case "hello":
        jp69 = 1
    case "world":
        jp69 = 2
    default:
        jp69 = 3
    }
    retv67 = jp69
    return retv67
}

func wildcard_position(s__1 string) int32 {
    var retv71 int32
    retv71 = 4
    return retv71
}

func repeated_string(s__2 string) int32 {
    var retv73 int32
    var jp75 int32
    switch s__2 {
    case "hello":
        jp75 = 6
    default:
        jp75 = 8
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var t77 int32 = match_string("hello")
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t77)
    println__T_string(t78)
    var t79 int32 = match_string("planet")
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t79)
    println__T_string(t80)
    var t81 int32 = wildcard_position("world")
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    println__T_string(t82)
    var t83 int32 = wildcard_position("sun")
    var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t83)
    println__T_string(t84)
    var t85 int32 = repeated_string("hello")
    var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t85)
    println__T_string(t86)
    var t87 int32 = repeated_string("mars")
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t87)
    println__T_string(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__5)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv97 string
    retv97 = self__37
    return retv97
}

func main() {
    main0()
}
