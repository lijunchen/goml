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
    var retv75 string
    var mtmp64 Point = self__0
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__2 int32 = x66
    var x__1 int32 = x65
    var t76 string = "Point { " + "x: "
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t78 string = t76 + t77
    var t79 string = t78 + ", "
    var t80 string = t79 + "y: "
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t82 string = t80 + t81
    var t83 string = t82 + " }"
    retv75 = t83
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv85 string
    var jp87 string
    switch self__3.(type) {
    case Quit:
        jp87 = "Message::Quit"
    case Move:
        var x67 int32 = self__3.(Move)._0
        var x68 int32 = self__3.(Move)._1
        var __field1__5 int32 = x68
        var __field0__4 int32 = x67
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t89 string = "Message::Move(" + t88
        var t90 string = t89 + ", "
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t92 string = t90 + t91
        var t93 string = t92 + ")"
        jp87 = t93
    case Write:
        var x69 string = self__3.(Write)._0
        var __field0__6 string = x69
        var t94 string = "Message::Write(" + __field0__6
        var t95 string = t94 + ")"
        jp87 = t95
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t97 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t97)
    var t98 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t98)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_int32_to_string(self__6)
    retv100 = t101
    return retv100
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv106 string
    retv106 = self__38
    return retv106
}

func main() {
    main0()
}
