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
    var retv69 int32
    var jp71 int32
    switch n__0 {
    case 0:
        jp71 = 10
    case 1:
        jp71 = 20
    default:
        jp71 = 30
    }
    retv69 = jp71
    return retv69
}

func wildcard_first(n__1 int32) int32 {
    var retv73 int32
    retv73 = 40
    return retv73
}

func wildcard_middle(n__2 int32) int32 {
    var retv75 int32
    var jp77 int32
    switch n__2 {
    case 2:
        jp77 = 90
    case 3:
        jp77 = 100
    default:
        jp77 = 100
    }
    retv75 = jp77
    return retv75
}

func repeated(n__3 int32) int32 {
    var retv79 int32
    var jp81 int32
    switch n__3 {
    case 1:
        jp81 = 60
    default:
        jp81 = 80
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t83 int32 = match_int(0)
    var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t83)
    println__T_string(t84)
    var t85 int32 = match_int(5)
    var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t85)
    println__T_string(t86)
    var t87 int32 = wildcard_first(0)
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t87)
    println__T_string(t88)
    var t89 int32 = wildcard_first(2)
    var t90 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t89)
    println__T_string(t90)
    var t91 int32 = wildcard_middle(2)
    var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t91)
    println__T_string(t92)
    var t93 int32 = wildcard_middle(3)
    var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t93)
    println__T_string(t94)
    var t95 int32 = repeated(1)
    var t96 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t95)
    println__T_string(t96)
    var t97 int32 = repeated(3)
    var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t97)
    println__T_string(t98)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int32_to_string(self__5)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv107 string
    retv107 = self__37
    return retv107
}

func main() {
    main0()
}
