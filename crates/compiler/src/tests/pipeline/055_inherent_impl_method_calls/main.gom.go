package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

func _goml_inherent_Shape_Shape_name(self__0 Shape) string {
    return "Shape"
}

func _goml_inherent_Shape_Shape_rename(self__1 Shape, suffix__2 string) string {
    var t2 string
    var t3 string
    t2 = _goml_inherent_Shape_Shape_name(self__1)
    t3 = t2 + suffix__2
    return t3
}

func _goml_inherent_Shape_Shape_join(self__3 Shape, left__4 string, right__5 string) string {
    var t4 string
    var t5 string
    var t6 string
    t4 = _goml_inherent_Shape_Shape_name(self__3)
    t5 = left__4 + t4
    t6 = t5 + right__5
    return t6
}

func announce(shape__6 Shape) struct{} {
    var base__7 string
    var with_suffix__8 string
    var combined__9 string
    base__7 = _goml_inherent_Shape_Shape_name(shape__6)
    with_suffix__8 = _goml_inherent_Shape_Shape_rename(shape__6, "!")
    combined__9 = _goml_inherent_Shape_Shape_join(shape__6, base__7, with_suffix__8)
    println__T_string(combined__9)
    return struct{}{}
}

func main0() struct{} {
    var shape__10 Shape
    shape__10 = Shape{}
    announce(shape__10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t7 struct{}
    t7 = string_println(value__1)
    return t7
}

func main() {
    main0()
}
