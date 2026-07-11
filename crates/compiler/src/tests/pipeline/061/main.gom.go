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
    var retv19 string
    var jp21 string
    switch self__0 {
    case Red:
        jp21 = "Color::Red"
    case Green:
        jp21 = "Color::Green"
    case Blue:
        jp21 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv19 = jp21
    return retv19
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv23 string
    var mtmp7 Point = self__1
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var x10 Color = mtmp7.color
    var color__4 Color = x10
    var y__3 int32 = x9
    var x__2 int32 = x8
    var t24 string = "Point { " + "x: "
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t26 string = t24 + t25
    var t27 string = t26 + ", "
    var t28 string = t27 + "y: "
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t30 string = t28 + t29
    var t31 string = t30 + ", "
    var t32 string = t31 + "color: "
    var t33 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t34 string = t32 + t33
    var t35 string = t34 + " }"
    retv23 = t35
    return retv23
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv37 Point
    var t38 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv37 = t38
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv40 string
    var mtmp11 Line = self__8
    var x12 Point = mtmp11.from
    var x13 Point = mtmp11.to
    var x14 Color = mtmp11.color
    var color__11 Color = x14
    var to__10 Point = x13
    var from__9 Point = x12
    var t41 string = "Line { " + "from: "
    var t42 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t43 string = t41 + t42
    var t44 string = t43 + ", "
    var t45 string = t44 + "to: "
    var t46 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t47 string = t45 + t46
    var t48 string = t47 + ", "
    var t49 string = t48 + "color: "
    var t50 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t51 string = t49 + t50
    var t52 string = t51 + " }"
    retv40 = t52
    return retv40
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv54 Line
    var t55 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv54 = t55
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv57 string
    var jp59 string
    switch self__15.(type) {
    case Nil:
        jp59 = "LineList::Nil"
    case Cons:
        var x15 Line = self__15.(Cons)._0
        var x16 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x16
        var __field0__16 Line = x15
        var t60 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t61 string = "LineList::Cons(" + t60
        var t62 string = t61 + ", "
        var t63 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t64 string = t62 + t63
        var t65 string = t64 + ")"
        jp59 = t65
    default:
        panic("non-exhaustive match")
    }
    retv57 = jp59
    return retv57
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t67 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t67)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv69 string
    var t70 string = _goml_runtime_core_int32_to_string(self__2)
    retv69 = t70
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv75 string
    retv75 = self__9
    return retv75
}

func main() {
    main0()
}
