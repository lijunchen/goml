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
    var retv19 string
    var mtmp7 Point = p__0
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__2 int32 = x9
    var x__1 int32 = x8
    var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t21 string = "Point { x: " + t20
    var t22 string = t21 + ", y: "
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t24 string = t22 + t23
    var t25 string = t24 + " }"
    retv19 = t25
    return retv19
}

func color_to_string(c__3 Color) string {
    var retv27 string
    var jp29 string
    switch c__3 {
    case Red:
        jp29 = "Red"
    case Green:
        jp29 = "Green"
    case Blue:
        jp29 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func line_to_string(l__4 Line) string {
    var retv31 string
    var mtmp10 Line = l__4
    var x11 Point = mtmp10.from
    var x12 Point = mtmp10.to
    var x13 Color = mtmp10.color
    var color__7 Color = x13
    var to__6 Point = x12
    var from__5 Point = x11
    var t32 string = point32_to_string(from__5)
    var t33 string = "Line { from: " + t32
    var t34 string = t33 + ", to: "
    var t35 string = point32_to_string(to__6)
    var t36 string = t34 + t35
    var t37 string = t36 + ", color: "
    var t38 string = color_to_string(color__7)
    var t39 string = t37 + t38
    var t40 string = t39 + " }"
    retv31 = t40
    return retv31
}

func point_type(p__8 Point) string {
    var retv42 string
    var x14 int32 = p__8.x
    var x15 int32 = p__8.y
    var jp44 string
    switch x14 {
    case 0:
        var jp46 string
        switch x15 {
        case 0:
            jp46 = "origin"
        case 1:
            jp46 = "up"
        default:
            var y__9 int32 = x15
            var mtmp16 bool = 0 < y__9
            var jp48 string
            switch mtmp16 {
            case true:
                jp48 = "above"
            case false:
                jp48 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp46 = jp48
        }
        jp44 = jp46
    case 1:
        var jp50 string
        switch x15 {
        case 0:
            jp50 = "right"
        default:
            jp50 = "unknown"
        }
        jp44 = jp50
    default:
        jp44 = "unknown"
    }
    retv42 = jp44
    return retv42
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t52 string = point_type(p0__10)
    println__T_string(t52)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t53 string = line_to_string(line__12)
    println__T_string(t53)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int32_to_string(self__2)
    retv56 = t57
    return retv56
}

func println__T_string(value__1 string) struct{} {
    var t59 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t59)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv62 string
    retv62 = self__9
    return retv62
}

func main() {
    main0()
}
