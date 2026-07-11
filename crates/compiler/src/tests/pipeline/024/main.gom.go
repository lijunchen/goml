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
    var retv34 string
    var mtmp22 Point = p__0
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__2 int32 = x24
    var x__1 int32 = x23
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t36 string = "Point { x: " + t35
    var t37 string = t36 + ", y: "
    var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t39 string = t37 + t38
    var t40 string = t39 + " }"
    retv34 = t40
    return retv34
}

func color_to_string(c__3 Color) string {
    var retv42 string
    var jp44 string
    switch c__3 {
    case Red:
        jp44 = "Red"
    case Green:
        jp44 = "Green"
    case Blue:
        jp44 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func line_to_string(l__4 Line) string {
    var retv46 string
    var mtmp25 Line = l__4
    var x26 Point = mtmp25.from
    var x27 Point = mtmp25.to
    var x28 Color = mtmp25.color
    var color__7 Color = x28
    var to__6 Point = x27
    var from__5 Point = x26
    var t47 string = point32_to_string(from__5)
    var t48 string = "Line { from: " + t47
    var t49 string = t48 + ", to: "
    var t50 string = point32_to_string(to__6)
    var t51 string = t49 + t50
    var t52 string = t51 + ", color: "
    var t53 string = color_to_string(color__7)
    var t54 string = t52 + t53
    var t55 string = t54 + " }"
    retv46 = t55
    return retv46
}

func point_type(p__8 Point) string {
    var retv57 string
    var x29 int32 = p__8.x
    var x30 int32 = p__8.y
    var jp59 string
    switch x29 {
    case 0:
        var jp61 string
        switch x30 {
        case 0:
            jp61 = "origin"
        case 1:
            jp61 = "up"
        default:
            var y__9 int32 = x30
            var mtmp31 bool = 0 < y__9
            var jp63 string
            switch mtmp31 {
            case true:
                jp63 = "above"
            case false:
                jp63 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp61 = jp63
        }
        jp59 = jp61
    case 1:
        var jp65 string
        switch x30 {
        case 0:
            jp65 = "right"
        default:
            jp65 = "unknown"
        }
        jp59 = jp65
    default:
        jp59 = "unknown"
    }
    retv57 = jp59
    return retv57
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t67 string = point_type(p0__10)
    println__T_string(t67)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t68 string = line_to_string(line__12)
    println__T_string(t68)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__2)
    retv71 = t72
    return retv71
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv77 string
    retv77 = self__9
    return retv77
}

func main() {
    main0()
}
