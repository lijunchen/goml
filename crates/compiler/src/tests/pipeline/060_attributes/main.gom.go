package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Point struct {
    x int32
    y int32
}

type Message interface {
    isMessage()
}

type Quit struct {}

func (_ Quit) isMessage() {}

type Move struct {
    _0 int32
    _1 int32
}

func (_ Move) isMessage() {}

type Write struct {
    _0 string
}

func (_ Write) isMessage() {}

func _goml_trait_impl_ToString_Point_to_string(self__0 Point) string {
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t10 string = "Point { " + "x: "
    var t11 string = int32_to_string(x__1)
    var t12 string = t10 + t11
    var t13 string = t12 + ", "
    var t14 string = t13 + "y: "
    var t15 string = int32_to_string(y__2)
    var t16 string = t14 + t15
    var t17 string = t16 + " }"
    return t17
}

func _goml_trait_impl_ToString_Message_to_string(self__3 Message) string {
    var jp19 string
    switch self__3.(type) {
    case Quit:
        jp19 = "Message::Quit"
    case Move:
        var x3 int32 = self__3.(Move)._0
        var x4 int32 = self__3.(Move)._1
        var __field1__5 int32 = x4
        var __field0__4 int32 = x3
        var t20 string = int32_to_string(__field0__4)
        var t21 string = "Message::Move(" + t20
        var t22 string = t21 + ", "
        var t23 string = int32_to_string(__field1__5)
        var t24 string = t22 + t23
        var t25 string = t24 + ")"
        jp19 = t25
    case Write:
        var x5 string = self__3.(Write)._0
        var __field0__6 string = x5
        var t26 string = "Message::Write(" + __field0__6
        var t27 string = t26 + ")"
        jp19 = t27
    default:
        panic("non-exhaustive match")
    }
    return jp19
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_trait_impl_ToString_Point_to_string(point__7)
    var t28 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_trait_impl_ToString_Message_to_string(t28)
    var t29 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_trait_impl_ToString_Message_to_string(t29)
    var exit__11 string = _goml_trait_impl_ToString_Message_to_string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t30 struct{} = string_println(value__1)
    return t30
}

func main() {
    main0()
}
