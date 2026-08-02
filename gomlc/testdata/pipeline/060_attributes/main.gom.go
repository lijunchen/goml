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
    var retv166 string
    var mtmp155 Point = self__0
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__2 int32 = x157
    var x__1 int32 = x156
    var t167 string = "Point { " + "x: "
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t169 string = t167 + t168
    var t170 string = t169 + ", "
    var t171 string = t170 + "y: "
    var t172 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t173 string = t171 + t172
    var t174 string = t173 + " }"
    retv166 = t174
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    var retv176 string
    var jp178 string
    switch self__3.(type) {
    case Quit:
        jp178 = "Message::Quit"
    case Move:
        var x158 int32 = self__3.(Move)._0
        var x159 int32 = self__3.(Move)._1
        var __field1__5 int32 = x159
        var __field0__4 int32 = x158
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__4)
        var t180 string = "Message::Move(" + t179
        var t181 string = t180 + ", "
        var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__5)
        var t183 string = t181 + t182
        var t184 string = t183 + ")"
        jp178 = t184
    case Write:
        var x160 string = self__3.(Write)._0
        var __field0__6 string = x160
        var t185 string = "Message::Write(" + __field0__6
        var t186 string = t185 + ")"
        jp178 = t186
    default:
        panic("non-exhaustive match")
    }
    retv176 = jp178
    return retv176
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 4,
        y: 7,
    }
    var summary__8 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(point__7)
    var t188 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t188)
    var t189 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t189)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    println__T_string(summary__8)
    println__T_string(mv__9)
    println__T_string(text__10)
    println__T_string(exit__11)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv191 string
    var t192 string = _goml_runtime_core_int32_to_string(self__6)
    retv191 = t192
    return retv191
}

func println__T_string(value__1 string) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv197 string
    retv197 = self__38
    return retv197
}

func main() {
    main0()
}
