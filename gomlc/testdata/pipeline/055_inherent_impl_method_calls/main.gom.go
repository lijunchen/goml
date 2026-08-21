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
    var t417 string
    t417 = "Shape"
    var t418 string = t417 + suffix__2
    return t418
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t421 string
    t421 = "Shape"
    var t422 string = left__4 + t421
    var t423 string = t422 + right__5
    return t423
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline446 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline447 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline448 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline446, inline447)
    println__T_string(inline448)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t427 string
    t427 = value__1
    _goml_runtime_core_string_println(t427)
    return struct{}{}
}

func main() {
    main0()
}
