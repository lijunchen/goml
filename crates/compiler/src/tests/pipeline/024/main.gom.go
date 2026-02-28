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
    var mtmp0 Point = p__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t11 string = int32_to_string(x__1)
    var t12 string = "Point { x: " + t11
    var t13 string = t12 + ", y: "
    var t14 string = int32_to_string(y__2)
    var t15 string = t13 + t14
    var t16 string = t15 + " }"
    return t16
}

func color_to_string(c__3 Color) string {
    var jp18 string
    switch c__3.(type) {
    case Red:
        jp18 = "Red"
    case Green:
        jp18 = "Green"
    case Blue:
        jp18 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    return jp18
}

func line_to_string(l__4 Line) string {
    var mtmp3 Line = l__4
    var x4 Point = mtmp3.from
    var x5 Point = mtmp3.to
    var x6 Color = mtmp3.color
    var color__7 Color = x6
    var to__6 Point = x5
    var from__5 Point = x4
    var t19 string = point32_to_string(from__5)
    var t20 string = "Line { from: " + t19
    var t21 string = t20 + ", to: "
    var t22 string = point32_to_string(to__6)
    var t23 string = t21 + t22
    var t24 string = t23 + ", color: "
    var t25 string = color_to_string(color__7)
    var t26 string = t24 + t25
    var t27 string = t26 + " }"
    return t27
}

func point_type(p__8 Point) string {
    var x7 int32 = p__8.x
    var x8 int32 = p__8.y
    var jp29 string
    switch x7 {
    case 0:
        var jp31 string
        switch x8 {
        case 0:
            jp31 = "origin"
            jp29 = jp31
            return jp29
        case 1:
            jp31 = "up"
            jp29 = jp31
            return jp29
        default:
            var y__9 int32 = x8
            var mtmp9 bool = 0 < y__9
            var jp33 string
            switch mtmp9 {
            case true:
                jp33 = "above"
            case false:
                jp33 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp31 = jp33
            jp29 = jp31
            return jp29
        }
    case 1:
        var jp35 string
        switch x8 {
        case 0:
            jp35 = "right"
        default:
            jp35 = "unknown"
        }
        jp29 = jp35
        return jp29
    default:
        jp29 = "unknown"
        return jp29
    }
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t36 string = point_type(p0__10)
    println__T_string(t36)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red{},
    }
    var t37 string = line_to_string(line__12)
    var t38 struct{} = println__T_string(t37)
    return t38
}

func println__T_string(value__1 string) struct{} {
    var t39 struct{} = string_println(value__1)
    return t39
}

func main() {
    main0()
}
