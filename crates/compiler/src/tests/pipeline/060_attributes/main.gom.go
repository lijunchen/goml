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

func impl_inherent_Point_to_string(self__0 Point) string {
    var ret26 string
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t14 string = "Point { " + "x: "
    var t15 string = int32_to_string(x__1)
    var t13 string = t14 + t15
    var t12 string = t13 + ", "
    var t11 string = t12 + "y: "
    var t16 string = int32_to_string(y__2)
    var t10 string = t11 + t16
    ret26 = t10 + " }"
    return ret26
}

func impl_inherent_Message_to_string(self__3 Message) string {
    var ret27 string
    switch self__3 := self__3.(type) {
    case Quit:
        ret27 = "Message::Quit"
    case Move:
        var x3 int32 = self__3._0
        var x4 int32 = self__3._1
        var __field1__5 int32 = x4
        var __field0__4 int32 = x3
        var t20 string = int32_to_string(__field0__4)
        var t19 string = "Message::Move(" + t20
        var t18 string = t19 + ", "
        var t21 string = int32_to_string(__field1__5)
        var t17 string = t18 + t21
        ret27 = t17 + ")"
    case Write:
        var x5 string = self__3._0
        var __field0__6 string = x5
        var t22 string = "Message::Write(" + __field0__6
        ret27 = t22 + ")"
    }
    return ret27
}

func main0() struct{} {
    var ret28 struct{}
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = impl_inherent_Point_to_string(point__7)
    var t23 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = impl_inherent_Message_to_string(t23)
    var t24 Message = Write{
        _0: "done",
    }
    var text__10 string = impl_inherent_Message_to_string(t24)
    var t25 Message = Quit{}
    var exit__11 string = impl_inherent_Message_to_string(t25)
    string_println(summary__8)
    string_println(mv__9)
    string_println(text__10)
    string_println(exit__11)
    ret28 = struct{}{}
    return ret28
}

func main() {
    main0()
}
