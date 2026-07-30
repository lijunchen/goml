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

type LeftSource struct {
    value int32
}

type RightSource struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_marked(self__0 int32) string {
    var retv110 string
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t112 string = "m" + t111
    retv110 = t112
    return retv110
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv114 int32
    var t115 int32 = self__1.value
    retv114 = t115
    return retv114
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv117 int32
    var t118 int32 = self__2.value
    retv117 = t118
    return retv117
}

func main0() struct{} {
    var t120 LeftSource = LeftSource{
        value: 3,
    }
    var t121 RightSource = RightSource{
        value: 4,
    }
    var t122 string = combine__A_LeftSource__B_RightSource(t120, t121)
    println__T_string(t122)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv124 string
    var t125 string = _goml_runtime_core_int32_to_string(self__6)
    retv124 = t125
    return retv124
}

func println__T_string(value__1 string) struct{} {
    var t127 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t127)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv130 string
    var t131 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t132 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t131)
    var t133 string = t132 + ":"
    var t134 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t135 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t134)
    var t136 string = t133 + t135
    retv130 = t136
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv138 string
    retv138 = self__38
    return retv138
}

func main() {
    main0()
}
