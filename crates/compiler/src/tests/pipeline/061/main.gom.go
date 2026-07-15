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
    var retv34 string
    var jp36 string
    switch self__0 {
    case Red:
        jp36 = "Color::Red"
    case Green:
        jp36 = "Color::Green"
    case Blue:
        jp36 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv34 = jp36
    return retv34
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv38 string
    var mtmp22 Point = self__1
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var x25 Color = mtmp22.color
    var color__4 Color = x25
    var y__3 int32 = x24
    var x__2 int32 = x23
    var t39 string = "Point { " + "x: "
    var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t41 string = t39 + t40
    var t42 string = t41 + ", "
    var t43 string = t42 + "y: "
    var t44 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t45 string = t43 + t44
    var t46 string = t45 + ", "
    var t47 string = t46 + "color: "
    var t48 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t49 string = t47 + t48
    var t50 string = t49 + " }"
    retv38 = t50
    return retv38
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv52 Point
    var t53 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv52 = t53
    return retv52
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv55 string
    var mtmp26 Line = self__8
    var x27 Point = mtmp26.from
    var x28 Point = mtmp26.to
    var x29 Color = mtmp26.color
    var color__11 Color = x29
    var to__10 Point = x28
    var from__9 Point = x27
    var t56 string = "Line { " + "from: "
    var t57 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t58 string = t56 + t57
    var t59 string = t58 + ", "
    var t60 string = t59 + "to: "
    var t61 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t62 string = t60 + t61
    var t63 string = t62 + ", "
    var t64 string = t63 + "color: "
    var t65 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t66 string = t64 + t65
    var t67 string = t66 + " }"
    retv55 = t67
    return retv55
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv69 Line
    var t70 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv72 string
    var jp74 string
    switch self__15.(type) {
    case Nil:
        jp74 = "LineList::Nil"
    case Cons:
        var x30 Line = self__15.(Cons)._0
        var x31 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x31
        var __field0__16 Line = x30
        var t75 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t76 string = "LineList::Cons(" + t75
        var t77 string = t76 + ", "
        var t78 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t79 string = t77 + t78
        var t80 string = t79 + ")"
        jp74 = t80
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t82 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__2)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv90 string
    retv90 = self__9
    return retv90
}

func main() {
    main0()
}
