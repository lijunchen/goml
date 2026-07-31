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
    var retv163 string
    var mtmp152 Point = self__0
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var y__2 int32 = x154
    var x__1 int32 = x153
    var t164 string = "Point { " + "x: "
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t166 string = t164 + t165
    var t167 string = t166 + ", "
    var t168 string = t167 + "y: "
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t170 string = t168 + t169
    var t171 string = t170 + " }"
    retv163 = t171
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv173 string
    var jp175 string
    switch self__3.(type) {
    case Quit:
        jp175 = "Message::Quit"
    case Move:
        var x155 int32 = self__3.(Move)._0
        var x156 int32 = self__3.(Move)._1
        var __field1__5 int32 = x156
        var __field0__4 int32 = x155
        var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t177 string = "Message::Move(" + t176
        var t178 string = t177 + ", "
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t180 string = t178 + t179
        var t181 string = t180 + ")"
        jp175 = t181
    case Write:
        var x157 string = self__3.(Write)._0
        var __field0__6 string = x157
        var t182 string = "Message::Write(" + __field0__6
        var t183 string = t182 + ")"
        jp175 = t183
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t185 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t185)
    var t186 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t186)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv188 string
    var t189 string = _goml_runtime_core_int32_to_string(self__6)
    retv188 = t189
    return retv188
}

func println__T_string(value__1 string) struct{} {
    var t191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv194 string
    retv194 = self__38
    return retv194
}

func main() {
    main0()
}
