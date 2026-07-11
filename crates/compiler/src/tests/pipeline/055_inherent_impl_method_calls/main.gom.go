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
    var retv7 string
    retv7 = "Shape"
    return retv7
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var retv9 string
    var t10 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__1)
    var t11 string = t10 + suffix__2
    retv9 = t11
    return retv9
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var retv13 string
    var t14 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__3)
    var t15 string = left__4 + t14
    var t16 string = t15 + right__5
    retv13 = t16
    return retv13
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
    var t20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv23 string
    retv23 = self__9
    return retv23
}

func main() {
    main0()
}
