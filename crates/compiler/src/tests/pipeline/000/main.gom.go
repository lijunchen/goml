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

func goml__inherent_x23_Color_x23_Color_x23_to__string(self__0 Color) string {
    var ret43 string
    switch self__0.(type) {
    case Red:
        ret43 = "Color::Red"
    case Green:
        ret43 = "Color::Green"
    case Blue:
        ret43 = "Color::Blue"
    }
    return ret43
}

func goml__inherent_x23_Point_x23_Point_x23_to__string(self__1 Point) string {
    var ret44 string
    var mtmp0 Point = self__1
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var x3 Color = mtmp0.color
    var color__4 Color = x3
    var y__3 int32 = x2
    var x__2 int32 = x1
    var t18 string = "Point { " + "x: "
    var t19 string = int32_to_string(x__2)
    var t17 string = t18 + t19
    var t16 string = t17 + ", "
    var t15 string = t16 + "y: "
    var t20 string = int32_to_string(y__3)
    var t14 string = t15 + t20
    var t13 string = t14 + ", "
    var t12 string = t13 + "color: "
    var t21 string = goml__inherent_x23_Color_x23_Color_x23_to__string(color__4)
    var t11 string = t12 + t21
    ret44 = t11 + " }"
    return ret44
}

func goml__inherent_x23_Point_x23_Point_x23_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var ret45 Point
    ret45 = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    return ret45
}

func goml__inherent_x23_Line_x23_Line_x23_to__string(self__8 Line) string {
    var ret46 string
    var mtmp4 Line = self__8
    var x5 Point = mtmp4.from
    var x6 Point = mtmp4.to
    var x7 Color = mtmp4.color
    var color__11 Color = x7
    var to__10 Point = x6
    var from__9 Point = x5
    var t29 string = "Line { " + "from: "
    var t30 string = goml__inherent_x23_Point_x23_Point_x23_to__string(from__9)
    var t28 string = t29 + t30
    var t27 string = t28 + ", "
    var t26 string = t27 + "to: "
    var t31 string = goml__inherent_x23_Point_x23_Point_x23_to__string(to__10)
    var t25 string = t26 + t31
    var t24 string = t25 + ", "
    var t23 string = t24 + "color: "
    var t32 string = goml__inherent_x23_Color_x23_Color_x23_to__string(color__11)
    var t22 string = t23 + t32
    ret46 = t22 + " }"
    return ret46
}

func goml__inherent_x23_Line_x23_Line_x23_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var ret47 Line
    ret47 = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    return ret47
}

func goml__inherent_x23_LineList_x23_LineList_x23_to__string(self__15 LineList) string {
    var ret48 string
    switch self__15 := self__15.(type) {
    case Nil:
        ret48 = "LineList::Nil"
    case Cons:
        var x8 Line = self__15._0
        var x9 LineList = self__15._1
        var __field1__17 LineList = x9
        var __field0__16 Line = x8
        var t36 string = goml__inherent_x23_Line_x23_Line_x23_to__string(__field0__16)
        var t35 string = "LineList::Cons(" + t36
        var t34 string = t35 + ", "
        var t37 string = goml__inherent_x23_LineList_x23_LineList_x23_to__string(__field1__17)
        var t33 string = t34 + t37
        ret48 = t33 + ")"
    }
    return ret48
}

func main0() struct{} {
    var ret49 struct{}
    var t38 Color = Red{}
    var from__18 Point = goml__inherent_x23_Point_x23_Point_x23_new(10, 20, t38)
    var t39 Color = Green{}
    var to__19 Point = goml__inherent_x23_Point_x23_Point_x23_new(30, 40, t39)
    var t40 Color = Blue{}
    var line__20 Line = goml__inherent_x23_Line_x23_Line_x23_new(from__18, to__19, t40)
    var t41 LineList = Nil{}
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: t41,
    }
    var t42 string = goml__inherent_x23_LineList_x23_LineList_x23_to__string(lines__21)
    string_println(t42)
    ret49 = struct{}{}
    return ret49
}

func main() {
    main0()
}
