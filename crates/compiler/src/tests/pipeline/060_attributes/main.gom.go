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
    var retv72 string
    var mtmp61 Point = self__0
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__2 int32 = x63
    var x__1 int32 = x62
    var t73 string = "Point { " + "x: "
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t75 string = t73 + t74
    var t76 string = t75 + ", "
    var t77 string = t76 + "y: "
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t79 string = t77 + t78
    var t80 string = t79 + " }"
    retv72 = t80
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv82 string
    var jp84 string
    switch self__3.(type) {
    case Quit:
        jp84 = "Message::Quit"
    case Move:
        var x64 int32 = self__3.(Move)._0
        var x65 int32 = self__3.(Move)._1
        var __field1__5 int32 = x65
        var __field0__4 int32 = x64
        var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t86 string = "Message::Move(" + t85
        var t87 string = t86 + ", "
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t89 string = t87 + t88
        var t90 string = t89 + ")"
        jp84 = t90
    case Write:
        var x66 string = self__3.(Write)._0
        var __field0__6 string = x66
        var t91 string = "Message::Write(" + __field0__6
        var t92 string = t91 + ")"
        jp84 = t92
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t94 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t94)
    var t95 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t95)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__5)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv103 string
    retv103 = self__37
    return retv103
}

func main() {
    main0()
}
