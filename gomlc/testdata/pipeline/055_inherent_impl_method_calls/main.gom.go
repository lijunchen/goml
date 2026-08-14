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
    var t193 string
    t193 = "Shape"
    var t194 string = t193 + suffix__2
    return t194
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t197 string
    t197 = "Shape"
    var t198 string = left__4 + t197
    var t199 string = t198 + right__5
    return t199
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline222 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline223 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline224 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline222, inline223)
    println__T_string(inline224)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t203 string
    t203 = value__1
    _goml_runtime_core_string_println(t203)
    return struct{}{}
}

func main() {
    main0()
}
