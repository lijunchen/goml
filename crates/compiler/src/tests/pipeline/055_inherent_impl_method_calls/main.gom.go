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
    var retv64 string
    retv64 = "Shape"
    return retv64
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var retv66 string
    var t67 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__1)
    var t68 string = t67 + suffix__2
    retv66 = t68
    return retv66
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var retv70 string
    var t71 string = _goml_m_inherent_i_Shape_i_Shape_i_name(self__3)
    var t72 string = left__4 + t71
    var t73 string = t72 + right__5
    retv70 = t73
    return retv70
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
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv80 string
    retv80 = self__37
    return retv80
}

func main() {
    main0()
}
