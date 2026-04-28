package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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

type GoError = error

func _goml_trait_impl_ToString_Color_to_string(self__0 Color) string {
    var retv12 string
    var jp14 string
    switch self__0 {
    case Red:
        jp14 = "Color::Red"
    case Green:
        jp14 = "Color::Green"
    case Blue:
        jp14 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func _goml_trait_impl_ToString_Point_to_string(self__1 Point) string {
    var retv16 string
    var mtmp0 Point = self__1
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var x3 Color = mtmp0.color
    var color__4 Color = x3
    var y__3 int32 = x2
    var x__2 int32 = x1
    var t17 string = "Point { " + "x: "
    var t18 string = int32_to_string(x__2)
    var t19 string = t17 + t18
    var t20 string = t19 + ", "
    var t21 string = t20 + "y: "
    var t22 string = int32_to_string(y__3)
    var t23 string = t21 + t22
    var t24 string = t23 + ", "
    var t25 string = t24 + "color: "
    var t26 string = _goml_trait_impl_ToString_Color_to_string(color__4)
    var t27 string = t25 + t26
    var t28 string = t27 + " }"
    retv16 = t28
    return retv16
}

func _goml_inherent_Point_Point_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv30 Point
    var t31 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv30 = t31
    return retv30
}

func _goml_trait_impl_ToString_Line_to_string(self__8 Line) string {
    var retv33 string
    var mtmp4 Line = self__8
    var x5 Point = mtmp4.from
    var x6 Point = mtmp4.to
    var x7 Color = mtmp4.color
    var color__11 Color = x7
    var to__10 Point = x6
    var from__9 Point = x5
    var t34 string = "Line { " + "from: "
    var t35 string = _goml_trait_impl_ToString_Point_to_string(from__9)
    var t36 string = t34 + t35
    var t37 string = t36 + ", "
    var t38 string = t37 + "to: "
    var t39 string = _goml_trait_impl_ToString_Point_to_string(to__10)
    var t40 string = t38 + t39
    var t41 string = t40 + ", "
    var t42 string = t41 + "color: "
    var t43 string = _goml_trait_impl_ToString_Color_to_string(color__11)
    var t44 string = t42 + t43
    var t45 string = t44 + " }"
    retv33 = t45
    return retv33
}

func _goml_inherent_Line_Line_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv47 Line
    var t48 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv47 = t48
    return retv47
}

func _goml_trait_impl_ToString_LineList_to_string(self__15 LineList) string {
    var retv50 string
    var jp52 string
    switch self__15.(type) {
    case Nil:
        jp52 = "LineList::Nil"
    case Cons:
        var x8 Line = self__15.(Cons)._0
        var x9 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x9
        var __field0__16 Line = x8
        var t53 string = _goml_trait_impl_ToString_Line_to_string(__field0__16)
        var t54 string = "LineList::Cons(" + t53
        var t55 string = t54 + ", "
        var t56 string = _goml_trait_impl_ToString_LineList_to_string(__field1__17)
        var t57 string = t55 + t56
        var t58 string = t57 + ")"
        jp52 = t58
    default:
        panic("non-exhaustive match")
    }
    retv50 = jp52
    return retv50
}

func main0() struct{} {
    var from__18 Point = _goml_inherent_Point_Point_new(10, 20, Red)
    var to__19 Point = _goml_inherent_Point_Point_new(30, 40, Green)
    var line__20 Line = _goml_inherent_Line_Line_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t60 string = _goml_trait_impl_ToString_LineList_to_string(lines__21)
    string_println(t60)
    return struct{}{}
}

func main() {
    main0()
}
