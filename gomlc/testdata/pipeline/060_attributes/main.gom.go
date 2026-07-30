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
    var retv119 string
    var mtmp108 Point = self__0
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var y__2 int32 = x110
    var x__1 int32 = x109
    var t120 string = "Point { " + "x: "
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t122 string = t120 + t121
    var t123 string = t122 + ", "
    var t124 string = t123 + "y: "
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t126 string = t124 + t125
    var t127 string = t126 + " }"
    retv119 = t127
    return retv119
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv129 string
    var jp131 string
    switch self__3.(type) {
    case Quit:
        jp131 = "Message::Quit"
    case Move:
        var x111 int32 = self__3.(Move)._0
        var x112 int32 = self__3.(Move)._1
        var __field1__5 int32 = x112
        var __field0__4 int32 = x111
        var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t133 string = "Message::Move(" + t132
        var t134 string = t133 + ", "
        var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t136 string = t134 + t135
        var t137 string = t136 + ")"
        jp131 = t137
    case Write:
        var x113 string = self__3.(Write)._0
        var __field0__6 string = x113
        var t138 string = "Message::Write(" + __field0__6
        var t139 string = t138 + ")"
        jp131 = t139
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t141 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t141)
    var t142 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t142)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv144 string
    var t145 string = _goml_runtime_core_int32_to_string(self__6)
    retv144 = t145
    return retv144
}

func println__T_string(value__1 string) struct{} {
    var t147 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t147)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv150 string
    retv150 = self__38
    return retv150
}

func main() {
    main0()
}
