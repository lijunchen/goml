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
    var t183 string
    t183 = "Shape"
    var t184 string = t183 + suffix__2
    return t184
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t187 string
    t187 = "Shape"
    var t188 string = left__4 + t187
    var t189 string = t188 + right__5
    return t189
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline212 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline213 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline214 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline212, inline213)
    println__T_string(inline214)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t193 string
    t193 = value__31
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func main() {
    main0()
}
