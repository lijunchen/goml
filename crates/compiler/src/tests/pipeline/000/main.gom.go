package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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

type Color interface {
    isColor()
}

type Red struct {}

func (_ Red) isColor() {}

type Green struct {}

func (_ Green) isColor() {}

type Blue struct {}

func (_ Blue) isColor() {}

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

func _goml_trait_impl_ToString_Color_to_string(self__0 Color) string {
    var jp12 string
    switch self__0.(type) {
    case Red:
        jp12 = "Color::Red"
    case Green:
        jp12 = "Color::Green"
    case Blue:
        jp12 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    return jp12
}

func _goml_trait_impl_ToString_Point_to_string(self__1 Point) string {
    var mtmp0 Point = self__1
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var x3 Color = mtmp0.color
    var color__4 Color = x3
    var y__3 int32 = x2
    var x__2 int32 = x1
    var t13 string = "Point { " + "x: "
    var t14 string = int32_to_string(x__2)
    var t15 string = t13 + t14
    var t16 string = t15 + ", "
    var t17 string = t16 + "y: "
    var t18 string = int32_to_string(y__3)
    var t19 string = t17 + t18
    var t20 string = t19 + ", "
    var t21 string = t20 + "color: "
    var t22 string = _goml_trait_impl_ToString_Color_to_string(color__4)
    var t23 string = t21 + t22
    var t24 string = t23 + " }"
    return t24
}

func _goml_inherent_Point_Point_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var t25 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    return t25
}

func _goml_trait_impl_ToString_Line_to_string(self__8 Line) string {
    var mtmp4 Line = self__8
    var x5 Point = mtmp4.from
    var x6 Point = mtmp4.to
    var x7 Color = mtmp4.color
    var color__11 Color = x7
    var to__10 Point = x6
    var from__9 Point = x5
    var t26 string = "Line { " + "from: "
    var t27 string = _goml_trait_impl_ToString_Point_to_string(from__9)
    var t28 string = t26 + t27
    var t29 string = t28 + ", "
    var t30 string = t29 + "to: "
    var t31 string = _goml_trait_impl_ToString_Point_to_string(to__10)
    var t32 string = t30 + t31
    var t33 string = t32 + ", "
    var t34 string = t33 + "color: "
    var t35 string = _goml_trait_impl_ToString_Color_to_string(color__11)
    var t36 string = t34 + t35
    var t37 string = t36 + " }"
    return t37
}

func _goml_inherent_Line_Line_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var t38 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    return t38
}

func _goml_trait_impl_ToString_LineList_to_string(self__15 LineList) string {
    var jp40 string
    switch self__15.(type) {
    case Nil:
        jp40 = "LineList::Nil"
    case Cons:
        var x8 Line = self__15.(Cons)._0
        var x9 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x9
        var __field0__16 Line = x8
        var t41 string = _goml_trait_impl_ToString_Line_to_string(__field0__16)
        var t42 string = "LineList::Cons(" + t41
        var t43 string = t42 + ", "
        var t44 string = _goml_trait_impl_ToString_LineList_to_string(__field1__17)
        var t45 string = t43 + t44
        var t46 string = t45 + ")"
        jp40 = t46
    default:
        panic("non-exhaustive match")
    }
    return jp40
}

func main0() struct{} {
    var from__18 Point = _goml_inherent_Point_Point_new(10, 20, Red{})
    var to__19 Point = _goml_inherent_Point_Point_new(30, 40, Green{})
    var line__20 Line = _goml_inherent_Line_Line_new(from__18, to__19, Blue{})
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t47 string = _goml_trait_impl_ToString_LineList_to_string(lines__21)
    string_println(t47)
    return struct{}{}
}

func main() {
    main0()
}
