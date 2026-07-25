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
    var retv76 string
    var mtmp64 Point = p__0
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__2 int32 = x66
    var x__1 int32 = x65
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t78 string = "Point { x: " + t77
    var t79 string = t78 + ", y: "
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t81 string = t79 + t80
    var t82 string = t81 + " }"
    retv76 = t82
    return retv76
}

func color_to_string(c__3 Color) string {
    var retv84 string
    var jp86 string
    switch c__3 {
    case Red:
        jp86 = "Red"
    case Green:
        jp86 = "Green"
    case Blue:
        jp86 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func line_to_string(l__4 Line) string {
    var retv88 string
    var mtmp67 Line = l__4
    var x68 Point = mtmp67.from
    var x69 Point = mtmp67.to
    var x70 Color = mtmp67.color
    var color__7 Color = x70
    var to__6 Point = x69
    var from__5 Point = x68
    var t89 string = point32_to_string(from__5)
    var t90 string = "Line { from: " + t89
    var t91 string = t90 + ", to: "
    var t92 string = point32_to_string(to__6)
    var t93 string = t91 + t92
    var t94 string = t93 + ", color: "
    var t95 string = color_to_string(color__7)
    var t96 string = t94 + t95
    var t97 string = t96 + " }"
    retv88 = t97
    return retv88
}

func point_type(p__8 Point) string {
    var retv99 string
    var x71 int32 = p__8.x
    var x72 int32 = p__8.y
    var jp101 string
    switch x71 {
    case 0:
        var jp103 string
        switch x72 {
        case 0:
            jp103 = "origin"
        case 1:
            jp103 = "up"
        default:
            var y__9 int32 = x72
            var mtmp73 bool = 0 < y__9
            var jp105 string
            switch mtmp73 {
            case true:
                jp105 = "above"
            case false:
                jp105 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp103 = jp105
        }
        jp101 = jp103
    case 1:
        var jp107 string
        switch x72 {
        case 0:
            jp107 = "right"
        default:
            jp107 = "unknown"
        }
        jp101 = jp107
    default:
        jp101 = "unknown"
    }
    retv99 = jp101
    return retv99
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t109 string = point_type(p0__10)
    println__T_string(t109)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t110 string = line_to_string(line__12)
    println__T_string(t110)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int32_to_string(self__6)
    retv113 = t114
    return retv113
}

func println__T_string(value__1 string) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv119 string
    retv119 = self__38
    return retv119
}

func main() {
    main0()
}
