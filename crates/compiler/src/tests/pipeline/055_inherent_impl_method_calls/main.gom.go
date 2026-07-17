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
    var retv61 string
    retv61 = "Shape"
    return retv61
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var retv63 string
    var t64 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__1)
    var t65 string = t64 + suffix__2
    retv63 = t65
    return retv63
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var retv67 string
    var t68 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__3)
    var t69 string = left__4 + t68
    var t70 string = t69 + right__5
    retv67 = t70
    return retv67
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
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv77 string
    retv77 = self__34
    return retv77
}

func main() {
    main0()
}
