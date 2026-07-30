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
    var retv120 string
    var jp122 string
    switch self__0 {
    case Red:
        jp122 = "Color::Red"
    case Green:
        jp122 = "Color::Green"
    case Blue:
        jp122 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv120 = jp122
    return retv120
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv124 string
    var mtmp108 Point = self__1
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var x111 Color = mtmp108.color
    var color__4 Color = x111
    var y__3 int32 = x110
    var x__2 int32 = x109
    var t125 string = "Point { " + "x: "
    var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t127 string = t125 + t126
    var t128 string = t127 + ", "
    var t129 string = t128 + "y: "
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t131 string = t129 + t130
    var t132 string = t131 + ", "
    var t133 string = t132 + "color: "
    var t134 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t135 string = t133 + t134
    var t136 string = t135 + " }"
    retv124 = t136
    return retv124
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv138 Point
    var t139 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv138 = t139
    return retv138
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv141 string
    var mtmp112 Line = self__8
    var x113 Point = mtmp112.from
    var x114 Point = mtmp112.to
    var x115 Color = mtmp112.color
    var color__11 Color = x115
    var to__10 Point = x114
    var from__9 Point = x113
    var t142 string = "Line { " + "from: "
    var t143 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t144 string = t142 + t143
    var t145 string = t144 + ", "
    var t146 string = t145 + "to: "
    var t147 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t148 string = t146 + t147
    var t149 string = t148 + ", "
    var t150 string = t149 + "color: "
    var t151 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t152 string = t150 + t151
    var t153 string = t152 + " }"
    retv141 = t153
    return retv141
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv155 Line
    var t156 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv158 string
    var jp160 string
    switch self__15.(type) {
    case Nil:
        jp160 = "LineList::Nil"
    case Cons:
        var x116 Line = self__15.(Cons)._0
        var x117 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x117
        var __field0__16 Line = x116
        var t161 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t162 string = "LineList::Cons(" + t161
        var t163 string = t162 + ", "
        var t164 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t165 string = t163 + t164
        var t166 string = t165 + ")"
        jp160 = t166
    default:
        panic("non-exhaustive match")
    }
    retv158 = jp160
    return retv158
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t168 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t168)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    retv170 = t171
    return retv170
}

func println__T_string(value__1 string) struct{} {
    var t173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv176 string
    retv176 = self__38
    return retv176
}

func main() {
    main0()
}
