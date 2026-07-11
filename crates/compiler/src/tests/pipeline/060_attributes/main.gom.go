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
    var retv18 string
    var mtmp7 Point = self__0
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__2 int32 = x9
    var x__1 int32 = x8
    var t19 string = "Point { " + "x: "
    var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t21 string = t19 + t20
    var t22 string = t21 + ", "
    var t23 string = t22 + "y: "
    var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t25 string = t23 + t24
    var t26 string = t25 + " }"
    retv18 = t26
    return retv18
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv28 string
    var jp30 string
    switch self__3.(type) {
    case Quit:
        jp30 = "Message::Quit"
    case Move:
        var x10 int32 = self__3.(Move)._0
        var x11 int32 = self__3.(Move)._1
        var __field1__5 int32 = x11
        var __field0__4 int32 = x10
        var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t32 string = "Message::Move(" + t31
        var t33 string = t32 + ", "
        var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t35 string = t33 + t34
        var t36 string = t35 + ")"
        jp30 = t36
    case Write:
        var x12 string = self__3.(Write)._0
        var __field0__6 string = x12
        var t37 string = "Message::Write(" + __field0__6
        var t38 string = t37 + ")"
        jp30 = t38
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t40 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t40)
    var t41 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t41)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__2)
    retv43 = t44
    return retv43
}

func println__T_string(value__1 string) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv49 string
    retv49 = self__9
    return retv49
}

func main() {
    main0()
}
