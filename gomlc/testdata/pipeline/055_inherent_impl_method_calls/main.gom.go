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
    var t178 string
    t178 = "Shape"
    var t179 string = t178 + suffix__2
    return t179
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t182 string
    t182 = "Shape"
    var t183 string = left__4 + t182
    var t184 string = t183 + right__5
    return t184
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline207 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline208 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline209 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline207, inline208)
    println__T_string(inline209)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t188 string
    t188 = value__31
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func main() {
    main0()
}
