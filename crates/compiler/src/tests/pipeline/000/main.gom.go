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
        goto b2
    case Green:
        goto b3
    case Blue:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp12
    b2:
    jp12 = "Color::Red"
    goto b1
    b3:
    jp12 = "Color::Green"
    goto b1
    b4:
    jp12 = "Color::Blue"
    goto b1
}

func _goml_trait_impl_ToString_Point_to_string(self__1 Point) string {
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var x3 Color
    var color__4 Color
    var y__3 int32
    var x__2 int32
    var t13 string
    var t14 string
    var t15 string
    var t16 string
    var t17 string
    var t18 string
    var t19 string
    var t20 string
    var t21 string
    var t22 string
    var t23 string
    var t24 string
    mtmp0 = self__1
    x1 = mtmp0.x
    x2 = mtmp0.y
    x3 = mtmp0.color
    color__4 = x3
    y__3 = x2
    x__2 = x1
    t13 = "Point { " + "x: "
    t14 = int32_to_string(x__2)
    t15 = t13 + t14
    t16 = t15 + ", "
    t17 = t16 + "y: "
    t18 = int32_to_string(y__3)
    t19 = t17 + t18
    t20 = t19 + ", "
    t21 = t20 + "color: "
    t22 = _goml_trait_impl_ToString_Color_to_string(color__4)
    t23 = t21 + t22
    t24 = t23 + " }"
    return t24
}

func _goml_inherent_Point_Point_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var t25 Point
    t25 = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    return t25
}

func _goml_trait_impl_ToString_Line_to_string(self__8 Line) string {
    var mtmp4 Line
    var x5 Point
    var x6 Point
    var x7 Color
    var color__11 Color
    var to__10 Point
    var from__9 Point
    var t26 string
    var t27 string
    var t28 string
    var t29 string
    var t30 string
    var t31 string
    var t32 string
    var t33 string
    var t34 string
    var t35 string
    var t36 string
    var t37 string
    mtmp4 = self__8
    x5 = mtmp4.from
    x6 = mtmp4.to
    x7 = mtmp4.color
    color__11 = x7
    to__10 = x6
    from__9 = x5
    t26 = "Line { " + "from: "
    t27 = _goml_trait_impl_ToString_Point_to_string(from__9)
    t28 = t26 + t27
    t29 = t28 + ", "
    t30 = t29 + "to: "
    t31 = _goml_trait_impl_ToString_Point_to_string(to__10)
    t32 = t30 + t31
    t33 = t32 + ", "
    t34 = t33 + "color: "
    t35 = _goml_trait_impl_ToString_Color_to_string(color__11)
    t36 = t34 + t35
    t37 = t36 + " }"
    return t37
}

func _goml_inherent_Line_Line_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var t38 Line
    t38 = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    return t38
}

func _goml_trait_impl_ToString_LineList_to_string(self__15 LineList) string {
    var jp40 string
    var x8 Line
    var x9 LineList
    var __field1__17 LineList
    var __field0__16 Line
    var t41 string
    var t42 string
    var t43 string
    var t44 string
    var t45 string
    var t46 string
    switch self__15.(type) {
    case Nil:
        goto b2
    case Cons:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp40
    b2:
    jp40 = "LineList::Nil"
    goto b1
    b3:
    x8 = self__15.(Cons)._0
    x9 = self__15.(Cons)._1
    __field1__17 = x9
    __field0__16 = x8
    t41 = _goml_trait_impl_ToString_Line_to_string(__field0__16)
    t42 = "LineList::Cons(" + t41
    t43 = t42 + ", "
    t44 = _goml_trait_impl_ToString_LineList_to_string(__field1__17)
    t45 = t43 + t44
    t46 = t45 + ")"
    jp40 = t46
    goto b1
}

func main0() struct{} {
    var from__18 Point
    var to__19 Point
    var line__20 Line
    var lines__21 LineList
    var t47 string
    from__18 = _goml_inherent_Point_Point_new(10, 20, Red{})
    to__19 = _goml_inherent_Point_Point_new(30, 40, Green{})
    line__20 = _goml_inherent_Line_Line_new(from__18, to__19, Blue{})
    lines__21 = Cons{
        _0: line__20,
        _1: Nil{},
    }
    t47 = _goml_trait_impl_ToString_LineList_to_string(lines__21)
    string_println(t47)
    return struct{}{}
}

func main() {
    main0()
}
