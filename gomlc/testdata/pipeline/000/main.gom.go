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
    var retv80 string
    var jp82 string
    switch self__0 {
    case Red:
        jp82 = "Color::Red"
    case Green:
        jp82 = "Color::Green"
    case Blue:
        jp82 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv80 = jp82
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv84 string
    var mtmp68 Point = self__1
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var x71 Color = mtmp68.color
    var color__4 Color = x71
    var y__3 int32 = x70
    var x__2 int32 = x69
    var t85 string = "Point { " + "x: "
    var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t87 string = t85 + t86
    var t88 string = t87 + ", "
    var t89 string = t88 + "y: "
    var t90 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t91 string = t89 + t90
    var t92 string = t91 + ", "
    var t93 string = t92 + "color: "
    var t94 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t95 string = t93 + t94
    var t96 string = t95 + " }"
    retv84 = t96
    return retv84
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv98 Point
    var t99 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv101 string
    var mtmp72 Line = self__8
    var x73 Point = mtmp72.from
    var x74 Point = mtmp72.to
    var x75 Color = mtmp72.color
    var color__11 Color = x75
    var to__10 Point = x74
    var from__9 Point = x73
    var t102 string = "Line { " + "from: "
    var t103 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t104 string = t102 + t103
    var t105 string = t104 + ", "
    var t106 string = t105 + "to: "
    var t107 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t108 string = t106 + t107
    var t109 string = t108 + ", "
    var t110 string = t109 + "color: "
    var t111 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t112 string = t110 + t111
    var t113 string = t112 + " }"
    retv101 = t113
    return retv101
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv115 Line
    var t116 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv118 string
    var jp120 string
    switch self__15.(type) {
    case Nil:
        jp120 = "LineList::Nil"
    case Cons:
        var x76 Line = self__15.(Cons)._0
        var x77 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x77
        var __field0__16 Line = x76
        var t121 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t122 string = "LineList::Cons(" + t121
        var t123 string = t122 + ", "
        var t124 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t125 string = t123 + t124
        var t126 string = t125 + ")"
        jp120 = t126
    default:
        panic("non-exhaustive match")
    }
    retv118 = jp120
    return retv118
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t128 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t128)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv130 string
    var t131 string = _goml_runtime_core_int32_to_string(self__6)
    retv130 = t131
    return retv130
}

func println__T_string(value__1 string) struct{} {
    var t133 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t133)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv136 string
    retv136 = self__38
    return retv136
}

func main() {
    main0()
}
