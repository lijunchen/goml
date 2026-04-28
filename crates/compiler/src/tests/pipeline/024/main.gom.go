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

type GoError = error

func point32_to_string(p__0 Point) string {
    var retv12 string
    var mtmp0 Point = p__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t13 string = int32_to_string(x__1)
    var t14 string = "Point { x: " + t13
    var t15 string = t14 + ", y: "
    var t16 string = int32_to_string(y__2)
    var t17 string = t15 + t16
    var t18 string = t17 + " }"
    retv12 = t18
    return retv12
}

func color_to_string(c__3 Color) string {
    var retv20 string
    var jp22 string
    switch c__3 {
    case Red:
        jp22 = "Red"
    case Green:
        jp22 = "Green"
    case Blue:
        jp22 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv20 = jp22
    return retv20
}

func line_to_string(l__4 Line) string {
    var retv24 string
    var mtmp3 Line = l__4
    var x4 Point = mtmp3.from
    var x5 Point = mtmp3.to
    var x6 Color = mtmp3.color
    var color__7 Color = x6
    var to__6 Point = x5
    var from__5 Point = x4
    var t25 string = point32_to_string(from__5)
    var t26 string = "Line { from: " + t25
    var t27 string = t26 + ", to: "
    var t28 string = point32_to_string(to__6)
    var t29 string = t27 + t28
    var t30 string = t29 + ", color: "
    var t31 string = color_to_string(color__7)
    var t32 string = t30 + t31
    var t33 string = t32 + " }"
    retv24 = t33
    return retv24
}

func point_type(p__8 Point) string {
    var retv35 string
    var x7 int32 = p__8.x
    var x8 int32 = p__8.y
    var jp37 string
    switch x7 {
    case 0:
        var jp39 string
        switch x8 {
        case 0:
            jp39 = "origin"
        case 1:
            jp39 = "up"
        default:
            var y__9 int32 = x8
            var mtmp9 bool = 0 < y__9
            var jp41 string
            switch mtmp9 {
            case true:
                jp41 = "above"
            case false:
                jp41 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp39 = jp41
        }
        jp37 = jp39
    case 1:
        var jp43 string
        switch x8 {
        case 0:
            jp43 = "right"
        default:
            jp43 = "unknown"
        }
        jp37 = jp43
    default:
        jp37 = "unknown"
    }
    retv35 = jp37
    return retv35
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t45 string = point_type(p0__10)
    println__T_string(t45)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t46 string = line_to_string(line__12)
    println__T_string(t46)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
