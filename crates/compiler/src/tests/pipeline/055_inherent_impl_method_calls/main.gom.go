package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

type GoError = error

func _goml_inherent_Shape_Shape_name(self__0 Shape) string {
    var retv3 string
    retv3 = "Shape"
    return retv3
}

func _goml_inherent_Shape_Shape_rename(self__1 Shape, suffix__2 string) string {
    var retv5 string
    var t6 string = _goml_inherent_Shape_Shape_name(self__1)
    var t7 string = t6 + suffix__2
    retv5 = t7
    return retv5
}

func _goml_inherent_Shape_Shape_join(self__3 Shape, left__4 string, right__5 string) string {
    var retv9 string
    var t10 string = _goml_inherent_Shape_Shape_name(self__3)
    var t11 string = left__4 + t10
    var t12 string = t11 + right__5
    retv9 = t12
    return retv9
}

func announce(shape__6 Shape) struct{} {
    var base__7 string = _goml_inherent_Shape_Shape_name(shape__6)
    var with_suffix__8 string = _goml_inherent_Shape_Shape_rename(shape__6, "!")
    var combined__9 string = _goml_inherent_Shape_Shape_join(shape__6, base__7, with_suffix__8)
    println__T_string(combined__9)
    return struct{}{}
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    announce(shape__10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
