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

func point32_to_string(p__0 Point) string {
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var t11 string
    var t12 string
    var t13 string
    var t14 string
    var t15 string
    var t16 string
    mtmp0 = p__0
    x1 = mtmp0.x
    x2 = mtmp0.y
    y__2 = x2
    x__1 = x1
    t11 = int32_to_string(x__1)
    t12 = "Point { x: " + t11
    t13 = t12 + ", y: "
    t14 = int32_to_string(y__2)
    t15 = t13 + t14
    t16 = t15 + " }"
    return t16
}

func color_to_string(c__3 Color) string {
    var jp18 string
    switch c__3.(type) {
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
    return jp18
    b2:
    jp18 = "Red"
    goto b1
    b3:
    jp18 = "Green"
    goto b1
    b4:
    jp18 = "Blue"
    goto b1
}

func line_to_string(l__4 Line) string {
    var mtmp3 Line
    var x4 Point
    var x5 Point
    var x6 Color
    var color__7 Color
    var to__6 Point
    var from__5 Point
    var t19 string
    var t20 string
    var t21 string
    var t22 string
    var t23 string
    var t24 string
    var t25 string
    var t26 string
    var t27 string
    mtmp3 = l__4
    x4 = mtmp3.from
    x5 = mtmp3.to
    x6 = mtmp3.color
    color__7 = x6
    to__6 = x5
    from__5 = x4
    t19 = point32_to_string(from__5)
    t20 = "Line { from: " + t19
    t21 = t20 + ", to: "
    t22 = point32_to_string(to__6)
    t23 = t21 + t22
    t24 = t23 + ", color: "
    t25 = color_to_string(color__7)
    t26 = t24 + t25
    t27 = t26 + " }"
    return t27
}

func point_type(p__8 Point) string {
    var x7 int32
    var x8 int32
    var jp29 string
    var jp31 string
    var y__9 int32
    var mtmp9 bool
    var jp33 string
    var jp35 string
    x7 = p__8.x
    x8 = p__8.y
    switch x7 {
    case 0:
        goto b2
    case 1:
        goto b10
    default:
        goto b14
    }
    b1:
    return jp29
    b2:
    switch x8 {
    case 0:
        goto b4
    case 1:
        goto b5
    default:
        goto b6
    }
    b3:
    jp29 = jp31
    goto b1
    b4:
    jp31 = "origin"
    goto b3
    b5:
    jp31 = "up"
    goto b3
    b6:
    y__9 = x8
    mtmp9 = 0 < y__9
    switch mtmp9 {
    case true:
        goto b8
    case false:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b7:
    jp31 = jp33
    goto b3
    b8:
    jp33 = "above"
    goto b7
    b9:
    jp33 = "below"
    goto b7
    b10:
    switch x8 {
    case 0:
        goto b12
    default:
        goto b13
    }
    b11:
    jp29 = jp35
    goto b1
    b12:
    jp35 = "right"
    goto b11
    b13:
    jp35 = "unknown"
    goto b11
    b14:
    jp29 = "unknown"
    goto b1
}

func main0() struct{} {
    var p0__10 Point
    var t36 string
    var p1__11 Point
    var line__12 Line
    var t37 string
    var t38 struct{}
    p0__10 = Point{
        x: 0,
        y: 0,
    }
    t36 = point_type(p0__10)
    println__T_string(t36)
    p1__11 = Point{
        x: 10,
        y: 10,
    }
    line__12 = Line{
        from: p0__10,
        to: p1__11,
        color: Red{},
    }
    t37 = line_to_string(line__12)
    t38 = println__T_string(t37)
    return t38
}

func println__T_string(value__1 string) struct{} {
    var t39 struct{}
    t39 = string_println(value__1)
    return t39
}

func main() {
    main0()
}
