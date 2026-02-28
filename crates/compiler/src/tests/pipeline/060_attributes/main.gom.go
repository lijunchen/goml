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
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var t10 string
    var t11 string
    var t12 string
    var t13 string
    var t14 string
    var t15 string
    var t16 string
    var t17 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            mtmp0 = self__0
            x1 = mtmp0.x
            x2 = mtmp0.y
            y__2 = x2
            x__1 = x1
            t10 = "Point { " + "x: "
            t11 = int32_to_string(x__1)
            t12 = t10 + t11
            t13 = t12 + ", "
            t14 = t13 + "y: "
            t15 = int32_to_string(y__2)
            t16 = t14 + t15
            t17 = t16 + " }"
            return t17
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_ToString_Message_to_string(self__3 Message) string {
    var jp19 string
    var x3 int32
    var x4 int32
    var __field1__5 int32
    var __field0__4 int32
    var t20 string
    var t21 string
    var t22 string
    var t23 string
    var t24 string
    var t25 string
    var x5 string
    var __field0__6 string
    var t26 string
    var t27 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch self__3.(type) {
            case Quit:
                pc = 2
            case Move:
                pc = 3
            case Write:
                pc = 4
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp19
        case 2:
            jp19 = "Message::Quit"
            pc = 1
        case 3:
            x3 = self__3.(Move)._0
            x4 = self__3.(Move)._1
            __field1__5 = x4
            __field0__4 = x3
            t20 = int32_to_string(__field0__4)
            t21 = "Message::Move(" + t20
            t22 = t21 + ", "
            t23 = int32_to_string(__field1__5)
            t24 = t22 + t23
            t25 = t24 + ")"
            jp19 = t25
            pc = 1
        case 4:
            x5 = self__3.(Write)._0
            __field0__6 = x5
            t26 = "Message::Write(" + __field0__6
            t27 = t26 + ")"
            jp19 = t27
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var point__7 Point
    var summary__8 string
    var t28 Message
    var mv__9 string
    var t29 Message
    var text__10 string
    var exit__11 string
    var _wild6 struct{}
    var _wild7 struct{}
    var _wild8 struct{}
    var _wild9 struct{}
    _ = _wild6
    _ = _wild7
    _ = _wild8
    _ = _wild9
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            point__7 = Point{
                x: 4,
                y: 7,
            }
            summary__8 = _goml_trait_impl_ToString_Point_to_string(point__7)
            t28 = Move{
                _0: 1,
                _1: 2,
            }
            mv__9 = _goml_trait_impl_ToString_Message_to_string(t28)
            t29 = Write{
                _0: "done",
            }
            text__10 = _goml_trait_impl_ToString_Message_to_string(t29)
            exit__11 = _goml_trait_impl_ToString_Message_to_string(Quit{})
            println__T_string(summary__8)
            println__T_string(mv__9)
            println__T_string(text__10)
            println__T_string(exit__11)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t30 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t30 = string_println(value__1)
            return t30
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
