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
    var retv116 int32
    var jp118 int32
    switch n__0 {
    case 0:
        jp118 = 10
    case 1:
        jp118 = 20
    default:
        jp118 = 30
    }
    retv116 = jp118
    return retv116
}

func wildcard_first(n__1 int32) int32 {
    var retv120 int32
    retv120 = 40
    return retv120
}

func wildcard_middle(n__2 int32) int32 {
    var retv122 int32
    var jp124 int32
    switch n__2 {
    case 2:
        jp124 = 90
    case 3:
        jp124 = 100
    default:
        jp124 = 100
    }
    retv122 = jp124
    return retv122
}

func repeated(n__3 int32) int32 {
    var retv126 int32
    var jp128 int32
    switch n__3 {
    case 1:
        jp128 = 60
    default:
        jp128 = 80
    }
    retv126 = jp128
    return retv126
}

func main0() struct{} {
    var t130 int32 = match_int(0)
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    println__T_string(t131)
    var t132 int32 = match_int(5)
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    println__T_string(t133)
    var t134 int32 = wildcard_first(0)
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t134)
    println__T_string(t135)
    var t136 int32 = wildcard_first(2)
    var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t136)
    println__T_string(t137)
    var t138 int32 = wildcard_middle(2)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t138)
    println__T_string(t139)
    var t140 int32 = wildcard_middle(3)
    var t141 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t140)
    println__T_string(t141)
    var t142 int32 = repeated(1)
    var t143 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t142)
    println__T_string(t143)
    var t144 int32 = repeated(3)
    var t145 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t144)
    println__T_string(t145)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int32_to_string(self__6)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv154 string
    retv154 = self__38
    return retv154
}

func main() {
    main0()
}
