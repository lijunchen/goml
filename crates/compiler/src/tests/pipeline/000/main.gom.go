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
    color Color
}

type Line struct {
    from Point
    to Point
    color Color
}

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

type LineList interface {
    isLineList()
}

type Nil struct {}

func (_ Nil) isLineList() {}

type Cons struct {
    _0 Line
    _1 LineList
}

func (_ Cons) isLineList() {}

func _goml_m_trait__impl_i_ToString_i_Color_i_to__string(self__0 Color) string {
    var retv70 string
    var jp72 string
    switch self__0 {
    case Red:
        jp72 = "Color::Red"
    case Green:
        jp72 = "Color::Green"
    case Blue:
        jp72 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv70 = jp72
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv74 string
    var mtmp58 Point = self__1
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var x61 Color = mtmp58.color
    var color__4 Color = x61
    var y__3 int32 = x60
    var x__2 int32 = x59
    var t75 string = "Point { " + "x: "
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t77 string = t75 + t76
    var t78 string = t77 + ", "
    var t79 string = t78 + "y: "
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t81 string = t79 + t80
    var t82 string = t81 + ", "
    var t83 string = t82 + "color: "
    var t84 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t85 string = t83 + t84
    var t86 string = t85 + " }"
    retv74 = t86
    return retv74
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv88 Point
    var t89 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv88 = t89
    return retv88
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv91 string
    var mtmp62 Line = self__8
    var x63 Point = mtmp62.from
    var x64 Point = mtmp62.to
    var x65 Color = mtmp62.color
    var color__11 Color = x65
    var to__10 Point = x64
    var from__9 Point = x63
    var t92 string = "Line { " + "from: "
    var t93 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t94 string = t92 + t93
    var t95 string = t94 + ", "
    var t96 string = t95 + "to: "
    var t97 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t98 string = t96 + t97
    var t99 string = t98 + ", "
    var t100 string = t99 + "color: "
    var t101 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t102 string = t100 + t101
    var t103 string = t102 + " }"
    retv91 = t103
    return retv91
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv105 Line
    var t106 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv105 = t106
    return retv105
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv108 string
    var jp110 string
    switch self__15.(type) {
    case Nil:
        jp110 = "LineList::Nil"
    case Cons:
        var x66 Line = self__15.(Cons)._0
        var x67 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x67
        var __field0__16 Line = x66
        var t111 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t112 string = "LineList::Cons(" + t111
        var t113 string = t112 + ", "
        var t114 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t115 string = t113 + t114
        var t116 string = t115 + ")"
        jp110 = t116
    default:
        panic("non-exhaustive match")
    }
    retv108 = jp110
    return retv108
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t118 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__2)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv126 string
    retv126 = self__34
    return retv126
}

func main() {
    main0()
}
