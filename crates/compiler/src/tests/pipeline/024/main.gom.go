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

func point32_to_string(p__0 Point) string {
    var retv16 string
    var mtmp4 Point = p__0
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__2 int32 = x6
    var x__1 int32 = x5
    var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t18 string = "Point { x: " + t17
    var t19 string = t18 + ", y: "
    var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t21 string = t19 + t20
    var t22 string = t21 + " }"
    retv16 = t22
    return retv16
}

func color_to_string(c__3 Color) string {
    var retv24 string
    var jp26 string
    switch c__3 {
    case Red:
        jp26 = "Red"
    case Green:
        jp26 = "Green"
    case Blue:
        jp26 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func line_to_string(l__4 Line) string {
    var retv28 string
    var mtmp7 Line = l__4
    var x8 Point = mtmp7.from
    var x9 Point = mtmp7.to
    var x10 Color = mtmp7.color
    var color__7 Color = x10
    var to__6 Point = x9
    var from__5 Point = x8
    var t29 string = point32_to_string(from__5)
    var t30 string = "Line { from: " + t29
    var t31 string = t30 + ", to: "
    var t32 string = point32_to_string(to__6)
    var t33 string = t31 + t32
    var t34 string = t33 + ", color: "
    var t35 string = color_to_string(color__7)
    var t36 string = t34 + t35
    var t37 string = t36 + " }"
    retv28 = t37
    return retv28
}

func point_type(p__8 Point) string {
    var retv39 string
    var x11 int32 = p__8.x
    var x12 int32 = p__8.y
    var jp41 string
    switch x11 {
    case 0:
        var jp43 string
        switch x12 {
        case 0:
            jp43 = "origin"
        case 1:
            jp43 = "up"
        default:
            var y__9 int32 = x12
            var mtmp13 bool = 0 < y__9
            var jp45 string
            switch mtmp13 {
            case true:
                jp45 = "above"
            case false:
                jp45 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp43 = jp45
        }
        jp41 = jp43
    case 1:
        var jp47 string
        switch x12 {
        case 0:
            jp47 = "right"
        default:
            jp47 = "unknown"
        }
        jp41 = jp47
    default:
        jp41 = "unknown"
    }
    retv39 = jp41
    return retv39
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t49 string = point_type(p0__10)
    println__T_string(t49)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t50 string = line_to_string(line__12)
    println__T_string(t50)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv53 string
    var t54 string = _goml_runtime_core_int32_to_string(self__2)
    retv53 = t54
    return retv53
}

func println__T_string(value__1 string) struct{} {
    var t56 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t56)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv59 string
    retv59 = self__9
    return retv59
}

func main() {
    main0()
}
