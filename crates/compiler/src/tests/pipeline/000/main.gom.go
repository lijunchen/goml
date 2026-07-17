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
    var retv73 string
    var jp75 string
    switch self__0 {
    case Red:
        jp75 = "Color::Red"
    case Green:
        jp75 = "Color::Green"
    case Blue:
        jp75 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv77 string
    var mtmp61 Point = self__1
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var x64 Color = mtmp61.color
    var color__4 Color = x64
    var y__3 int32 = x63
    var x__2 int32 = x62
    var t78 string = "Point { " + "x: "
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t80 string = t78 + t79
    var t81 string = t80 + ", "
    var t82 string = t81 + "y: "
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t84 string = t82 + t83
    var t85 string = t84 + ", "
    var t86 string = t85 + "color: "
    var t87 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t88 string = t86 + t87
    var t89 string = t88 + " }"
    retv77 = t89
    return retv77
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv91 Point
    var t92 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv94 string
    var mtmp65 Line = self__8
    var x66 Point = mtmp65.from
    var x67 Point = mtmp65.to
    var x68 Color = mtmp65.color
    var color__11 Color = x68
    var to__10 Point = x67
    var from__9 Point = x66
    var t95 string = "Line { " + "from: "
    var t96 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t97 string = t95 + t96
    var t98 string = t97 + ", "
    var t99 string = t98 + "to: "
    var t100 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t101 string = t99 + t100
    var t102 string = t101 + ", "
    var t103 string = t102 + "color: "
    var t104 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t105 string = t103 + t104
    var t106 string = t105 + " }"
    retv94 = t106
    return retv94
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv108 Line
    var t109 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv108 = t109
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv111 string
    var jp113 string
    switch self__15.(type) {
    case Nil:
        jp113 = "LineList::Nil"
    case Cons:
        var x69 Line = self__15.(Cons)._0
        var x70 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x70
        var __field0__16 Line = x69
        var t114 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t115 string = "LineList::Cons(" + t114
        var t116 string = t115 + ", "
        var t117 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t118 string = t116 + t117
        var t119 string = t118 + ")"
        jp113 = t119
    default:
        panic("non-exhaustive match")
    }
    retv111 = jp113
    return retv111
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t121 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t121)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int32_to_string(self__5)
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv129 string
    retv129 = self__37
    return retv129
}

func main() {
    main0()
}
