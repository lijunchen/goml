package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__0 Point) string {
    var retv15 string
    var mtmp4 Point = self__0
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__2 int32 = x6
    var x__1 int32 = x5
    var t16 string = "Point { " + "x: "
    var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t18 string = t16 + t17
    var t19 string = t18 + ", "
    var t20 string = t19 + "y: "
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t22 string = t20 + t21
    var t23 string = t22 + " }"
    retv15 = t23
    return retv15
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv25 string
    var jp27 string
    switch self__3.(type) {
    case Quit:
        jp27 = "Message::Quit"
    case Move:
        var x7 int32 = self__3.(Move)._0
        var x8 int32 = self__3.(Move)._1
        var __field1__5 int32 = x8
        var __field0__4 int32 = x7
        var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t29 string = "Message::Move(" + t28
        var t30 string = t29 + ", "
        var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t32 string = t30 + t31
        var t33 string = t32 + ")"
        jp27 = t33
    case Write:
        var x9 string = self__3.(Write)._0
        var __field0__6 string = x9
        var t34 string = "Message::Write(" + __field0__6
        var t35 string = t34 + ")"
        jp27 = t35
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t37 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t37)
    var t38 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t38)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func println__T_string(value__1 string) struct{} {
    var t43 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t43)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv46 string
    retv46 = self__9
    return retv46
}

func main() {
    main0()
}
