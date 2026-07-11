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
    var retv33 string
    var mtmp22 Point = self__0
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__2 int32 = x24
    var x__1 int32 = x23
    var t34 string = "Point { " + "x: "
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t36 string = t34 + t35
    var t37 string = t36 + ", "
    var t38 string = t37 + "y: "
    var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t40 string = t38 + t39
    var t41 string = t40 + " }"
    retv33 = t41
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv43 string
    var jp45 string
    switch self__3.(type) {
    case Quit:
        jp45 = "Message::Quit"
    case Move:
        var x25 int32 = self__3.(Move)._0
        var x26 int32 = self__3.(Move)._1
        var __field1__5 int32 = x26
        var __field0__4 int32 = x25
        var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t47 string = "Message::Move(" + t46
        var t48 string = t47 + ", "
        var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t50 string = t48 + t49
        var t51 string = t50 + ")"
        jp45 = t51
    case Write:
        var x27 string = self__3.(Write)._0
        var __field0__6 string = x27
        var t52 string = "Message::Write(" + __field0__6
        var t53 string = t52 + ")"
        jp45 = t53
    default:
        panic("non-exhaustive match")
    }
    retv43 = jp45
    return retv43
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t55 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t55)
    var t56 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t56)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv58 string
    var t59 string = _goml_runtime_core_int32_to_string(self__2)
    retv58 = t59
    return retv58
}

func println__T_string(value__1 string) struct{} {
    var t61 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t61)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv64 string
    retv64 = self__9
    return retv64
}

func main() {
    main0()
}
