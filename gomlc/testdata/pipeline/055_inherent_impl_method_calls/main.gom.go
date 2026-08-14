package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

type Ordering int32

func _goml_m_inherent_i_Shape_i_Shape_i_name(self__0 Shape) string {
    return "Shape"
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var t414 string
    t414 = "Shape"
    var t415 string = t414 + suffix__2
    return t415
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t418 string
    t418 = "Shape"
    var t419 string = left__4 + t418
    var t420 string = t419 + right__5
    return t420
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline443 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline444 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline445 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline443, inline444)
    println__T_string(inline445)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t424 string
    t424 = value__1
    _goml_runtime_core_string_println(t424)
    return struct{}{}
}

func main() {
    main0()
}
