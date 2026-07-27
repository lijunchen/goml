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
    var retv76 string
    var jp78 string
    switch self__0 {
    case Red:
        jp78 = "Color::Red"
    case Green:
        jp78 = "Color::Green"
    case Blue:
        jp78 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv80 string
    var mtmp64 Point = self__1
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var x67 Color = mtmp64.color
    var color__4 Color = x67
    var y__3 int32 = x66
    var x__2 int32 = x65
    var t81 string = "Point { " + "x: "
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t83 string = t81 + t82
    var t84 string = t83 + ", "
    var t85 string = t84 + "y: "
    var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t87 string = t85 + t86
    var t88 string = t87 + ", "
    var t89 string = t88 + "color: "
    var t90 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t91 string = t89 + t90
    var t92 string = t91 + " }"
    retv80 = t92
    return retv80
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv94 Point
    var t95 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv97 string
    var mtmp68 Line = self__8
    var x69 Point = mtmp68.from
    var x70 Point = mtmp68.to
    var x71 Color = mtmp68.color
    var color__11 Color = x71
    var to__10 Point = x70
    var from__9 Point = x69
    var t98 string = "Line { " + "from: "
    var t99 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t100 string = t98 + t99
    var t101 string = t100 + ", "
    var t102 string = t101 + "to: "
    var t103 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t104 string = t102 + t103
    var t105 string = t104 + ", "
    var t106 string = t105 + "color: "
    var t107 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t108 string = t106 + t107
    var t109 string = t108 + " }"
    retv97 = t109
    return retv97
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv111 Line
    var t112 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv114 string
    var jp116 string
    switch self__15.(type) {
    case Nil:
        jp116 = "LineList::Nil"
    case Cons:
        var x72 Line = self__15.(Cons)._0
        var x73 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x73
        var __field0__16 Line = x72
        var t117 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t118 string = "LineList::Cons(" + t117
        var t119 string = t118 + ", "
        var t120 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t121 string = t119 + t120
        var t122 string = t121 + ")"
        jp116 = t122
    default:
        panic("non-exhaustive match")
    }
    retv114 = jp116
    return retv114
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t124 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t124)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__6)
    retv126 = t127
    return retv126
}

func println__T_string(value__1 string) struct{} {
    var t129 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t129)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv132 string
    retv132 = self__38
    return retv132
}

func main() {
    main0()
}
