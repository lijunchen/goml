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
    var retv16 string
    var jp18 string
    switch self__0 {
    case Red:
        jp18 = "Color::Red"
    case Green:
        jp18 = "Color::Green"
    case Blue:
        jp18 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv16 = jp18
    return retv16
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv20 string
    var mtmp4 Point = self__1
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var x7 Color = mtmp4.color
    var color__4 Color = x7
    var y__3 int32 = x6
    var x__2 int32 = x5
    var t21 string = "Point { " + "x: "
    var t22 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t23 string = t21 + t22
    var t24 string = t23 + ", "
    var t25 string = t24 + "y: "
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t27 string = t25 + t26
    var t28 string = t27 + ", "
    var t29 string = t28 + "color: "
    var t30 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t31 string = t29 + t30
    var t32 string = t31 + " }"
    retv20 = t32
    return retv20
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv34 Point
    var t35 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv34 = t35
    return retv34
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv37 string
    var mtmp8 Line = self__8
    var x9 Point = mtmp8.from
    var x10 Point = mtmp8.to
    var x11 Color = mtmp8.color
    var color__11 Color = x11
    var to__10 Point = x10
    var from__9 Point = x9
    var t38 string = "Line { " + "from: "
    var t39 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t40 string = t38 + t39
    var t41 string = t40 + ", "
    var t42 string = t41 + "to: "
    var t43 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t44 string = t42 + t43
    var t45 string = t44 + ", "
    var t46 string = t45 + "color: "
    var t47 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t48 string = t46 + t47
    var t49 string = t48 + " }"
    retv37 = t49
    return retv37
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv51 Line
    var t52 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv51 = t52
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv54 string
    var jp56 string
    switch self__15.(type) {
    case Nil:
        jp56 = "LineList::Nil"
    case Cons:
        var x12 Line = self__15.(Cons)._0
        var x13 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x13
        var __field0__16 Line = x12
        var t57 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t58 string = "LineList::Cons(" + t57
        var t59 string = t58 + ", "
        var t60 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t61 string = t59 + t60
        var t62 string = t61 + ")"
        jp56 = t62
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t64 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t64)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv66 string
    var t67 string = _goml_runtime_core_int32_to_string(self__2)
    retv66 = t67
    return retv66
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv72 string
    retv72 = self__9
    return retv72
}

func main() {
    main0()
}
