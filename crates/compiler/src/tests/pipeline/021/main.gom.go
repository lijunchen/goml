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
    var retv66 int32
    var jp68 int32
    switch n__0 {
    case 0:
        jp68 = 10
    case 1:
        jp68 = 20
    default:
        jp68 = 30
    }
    retv66 = jp68
    return retv66
}

func wildcard_first(n__1 int32) int32 {
    var retv70 int32
    retv70 = 40
    return retv70
}

func wildcard_middle(n__2 int32) int32 {
    var retv72 int32
    var jp74 int32
    switch n__2 {
    case 2:
        jp74 = 90
    case 3:
        jp74 = 100
    default:
        jp74 = 100
    }
    retv72 = jp74
    return retv72
}

func repeated(n__3 int32) int32 {
    var retv76 int32
    var jp78 int32
    switch n__3 {
    case 1:
        jp78 = 60
    default:
        jp78 = 80
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t80 int32 = match_int(0)
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    println__T_string(t81)
    var t82 int32 = match_int(5)
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    var t84 int32 = wildcard_first(0)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 int32 = wildcard_first(2)
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    println__T_string(t87)
    var t88 int32 = wildcard_middle(2)
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    println__T_string(t89)
    var t90 int32 = wildcard_middle(3)
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    println__T_string(t91)
    var t92 int32 = repeated(1)
    var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t92)
    println__T_string(t93)
    var t94 int32 = repeated(3)
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t94)
    println__T_string(t95)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__2)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv104 string
    retv104 = self__34
    return retv104
}

func main() {
    main0()
}
