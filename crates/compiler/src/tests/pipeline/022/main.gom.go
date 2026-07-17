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
    var retv64 int32
    var jp66 int32
    switch s__0 {
    case "hello":
        jp66 = 1
    case "world":
        jp66 = 2
    default:
        jp66 = 3
    }
    retv64 = jp66
    return retv64
}

func wildcard_position(s__1 string) int32 {
    var retv68 int32
    retv68 = 4
    return retv68
}

func repeated_string(s__2 string) int32 {
    var retv70 int32
    var jp72 int32
    switch s__2 {
    case "hello":
        jp72 = 6
    default:
        jp72 = 8
    }
    retv70 = jp72
    return retv70
}

func main0() struct{} {
    var t74 int32 = match_string("hello")
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t74)
    println__T_string(t75)
    var t76 int32 = match_string("planet")
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    println__T_string(t77)
    var t78 int32 = wildcard_position("world")
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    var t80 int32 = wildcard_position("sun")
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    println__T_string(t81)
    var t82 int32 = repeated_string("hello")
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    var t84 int32 = repeated_string("mars")
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__2)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv94 string
    retv94 = self__34
    return retv94
}

func main() {
    main0()
}
