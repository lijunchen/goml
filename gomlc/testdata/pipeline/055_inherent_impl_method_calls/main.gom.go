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
    var t161 string
    t161 = "Shape"
    var t162 string = t161 + suffix__2
    return t162
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t165 string
    t165 = "Shape"
    var t166 string = left__4 + t165
    var t167 string = t166 + right__5
    return t167
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline190 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline191 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline192 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline190, inline191)
    println__T_string(inline192)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t171 string
    t171 = value__1
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func main() {
    main0()
}
