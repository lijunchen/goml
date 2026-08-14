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
    return "Shape"
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var t188 string
    t188 = "Shape"
    var t189 string = t188 + suffix__2
    return t189
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t192 string
    t192 = "Shape"
    var t193 string = left__4 + t192
    var t194 string = t193 + right__5
    return t194
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline217 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline218 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline219 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline217, inline218)
    println__T_string(inline219)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t198 string
    t198 = value__1
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func main() {
    main0()
}
