package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

func impl_inherent_Shape_name(self__0 Shape) string {
    var ret5 string
    ret5 = "Shape"
    return ret5
}

func impl_inherent_Shape_rename(self__1 Shape, suffix__2 string) string {
    var ret6 string
    var t2 string = impl_inherent_Shape_name(self__1)
    ret6 = t2 + suffix__2
    return ret6
}

func impl_inherent_Shape_join(self__3 Shape, left__4 string, right__5 string) string {
    var ret7 string
    var t4 string = impl_inherent_Shape_name(self__3)
    var t3 string = left__4 + t4
    ret7 = t3 + right__5
    return ret7
}

func announce(shape__6 Shape) struct{} {
    var ret8 struct{}
    var base__7 string = impl_inherent_Shape_name(shape__6)
    var with_suffix__8 string = impl_inherent_Shape_rename(shape__6, "!")
    var combined__9 string = impl_inherent_Shape_join(shape__6, base__7, with_suffix__8)
    string_println(combined__9)
    ret8 = struct{}{}
    return ret8
}

func main0() struct{} {
    var ret9 struct{}
    var shape__10 Shape = Shape{}
    announce(shape__10)
    ret9 = struct{}{}
    return ret9
}

func main() {
    main0()
}
