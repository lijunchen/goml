package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

func _goml_m_inherent_i_Shape_i_Shape_i_name(self__0 Shape) string {
    var retv111 string
    retv111 = "Shape"
    return retv111
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var retv113 string
    var t114 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__1)
    var t115 string = t114 + suffix__2
    retv113 = t115
    return retv113
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var retv117 string
    var t118 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__3)
    var t119 string = left__4 + t118
    var t120 string = t119 + right__5
    retv117 = t120
    return retv117
}

func announce(shape__6 Shape) struct{} {
    var base__7 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__6)
    var with_suffix__8 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__6, "!")
    var combined__9 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__6, base__7, with_suffix__8)
    println__T_string(combined__9)
    return struct{}{}
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    announce(shape__10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t124 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t124)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv127 string
    retv127 = self__38
    return retv127
}

func main() {
    main0()
}
