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
    var retv69 string
    var mtmp58 Point = self__0
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var y__2 int32 = x60
    var x__1 int32 = x59
    var t70 string = "Point { " + "x: "
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t72 string = t70 + t71
    var t73 string = t72 + ", "
    var t74 string = t73 + "y: "
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t76 string = t74 + t75
    var t77 string = t76 + " }"
    retv69 = t77
    return retv69
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv79 string
    var jp81 string
    switch self__3.(type) {
    case Quit:
        jp81 = "Message::Quit"
    case Move:
        var x61 int32 = self__3.(Move)._0
        var x62 int32 = self__3.(Move)._1
        var __field1__5 int32 = x62
        var __field0__4 int32 = x61
        var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t83 string = "Message::Move(" + t82
        var t84 string = t83 + ", "
        var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t86 string = t84 + t85
        var t87 string = t86 + ")"
        jp81 = t87
    case Write:
        var x63 string = self__3.(Write)._0
        var __field0__6 string = x63
        var t88 string = "Message::Write(" + __field0__6
        var t89 string = t88 + ")"
        jp81 = t89
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t91 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t91)
    var t92 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t92)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__2)
    retv94 = t95
    return retv94
}

func println__T_string(value__1 string) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv100 string
    retv100 = self__34
    return retv100
}

func main() {
    main0()
}
