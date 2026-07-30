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
    var retv76 int32
    var jp78 int32
    switch n__0 {
    case 0:
        jp78 = 10
    case 1:
        jp78 = 20
    default:
        jp78 = 30
    }
    retv76 = jp78
    return retv76
}

func wildcard_first(n__1 int32) int32 {
    var retv80 int32
    retv80 = 40
    return retv80
}

func wildcard_middle(n__2 int32) int32 {
    var retv82 int32
    var jp84 int32
    switch n__2 {
    case 2:
        jp84 = 90
    case 3:
        jp84 = 100
    default:
        jp84 = 100
    }
    retv82 = jp84
    return retv82
}

func repeated(n__3 int32) int32 {
    var retv86 int32
    var jp88 int32
    switch n__3 {
    case 1:
        jp88 = 60
    default:
        jp88 = 80
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t90 int32 = match_int(0)
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    println__T_string(t91)
    var t92 int32 = match_int(5)
    var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t92)
    println__T_string(t93)
    var t94 int32 = wildcard_first(0)
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t94)
    println__T_string(t95)
    var t96 int32 = wildcard_first(2)
    var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t96)
    println__T_string(t97)
    var t98 int32 = wildcard_middle(2)
    var t99 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t98)
    println__T_string(t99)
    var t100 int32 = wildcard_middle(3)
    var t101 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t100)
    println__T_string(t101)
    var t102 int32 = repeated(1)
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t102)
    println__T_string(t103)
    var t104 int32 = repeated(3)
    var t105 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t104)
    println__T_string(t105)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv111 string
    var t112 string = _goml_runtime_core_int32_to_string(self__6)
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func main() {
    main0()
}
