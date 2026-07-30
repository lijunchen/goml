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
    var retv79 string
    var mtmp68 Point = self__0
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__2 int32 = x70
    var x__1 int32 = x69
    var t80 string = "Point { " + "x: "
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t82 string = t80 + t81
    var t83 string = t82 + ", "
    var t84 string = t83 + "y: "
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t86 string = t84 + t85
    var t87 string = t86 + " }"
    retv79 = t87
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv89 string
    var jp91 string
    switch self__3.(type) {
    case Quit:
        jp91 = "Message::Quit"
    case Move:
        var x71 int32 = self__3.(Move)._0
        var x72 int32 = self__3.(Move)._1
        var __field1__5 int32 = x72
        var __field0__4 int32 = x71
        var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t93 string = "Message::Move(" + t92
        var t94 string = t93 + ", "
        var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t96 string = t94 + t95
        var t97 string = t96 + ")"
        jp91 = t97
    case Write:
        var x73 string = self__3.(Write)._0
        var __field0__6 string = x73
        var t98 string = "Message::Write(" + __field0__6
        var t99 string = t98 + ")"
        jp91 = t99
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t101 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t101)
    var t102 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t102)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int32_to_string(self__6)
    retv104 = t105
    return retv104
}

func println__T_string(value__1 string) struct{} {
    var t107 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t107)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv110 string
    retv110 = self__38
    return retv110
}

func main() {
    main0()
}
