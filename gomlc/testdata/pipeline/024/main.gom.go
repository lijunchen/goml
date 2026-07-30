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
    var retv80 string
    var mtmp68 Point = p__0
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__2 int32 = x70
    var x__1 int32 = x69
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t82 string = "Point { x: " + t81
    var t83 string = t82 + ", y: "
    var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t85 string = t83 + t84
    var t86 string = t85 + " }"
    retv80 = t86
    return retv80
}

func color_to_string(c__3 Color) string {
    var retv88 string
    var jp90 string
    switch c__3 {
    case Red:
        jp90 = "Red"
    case Green:
        jp90 = "Green"
    case Blue:
        jp90 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func line_to_string(l__4 Line) string {
    var retv92 string
    var mtmp71 Line = l__4
    var x72 Point = mtmp71.from
    var x73 Point = mtmp71.to
    var x74 Color = mtmp71.color
    var color__7 Color = x74
    var to__6 Point = x73
    var from__5 Point = x72
    var t93 string = point32_to_string(from__5)
    var t94 string = "Line { from: " + t93
    var t95 string = t94 + ", to: "
    var t96 string = point32_to_string(to__6)
    var t97 string = t95 + t96
    var t98 string = t97 + ", color: "
    var t99 string = color_to_string(color__7)
    var t100 string = t98 + t99
    var t101 string = t100 + " }"
    retv92 = t101
    return retv92
}

func point_type(p__8 Point) string {
    var retv103 string
    var x75 int32 = p__8.x
    var x76 int32 = p__8.y
    var jp105 string
    switch x75 {
    case 0:
        var jp107 string
        switch x76 {
        case 0:
            jp107 = "origin"
        case 1:
            jp107 = "up"
        default:
            var y__9 int32 = x76
            var mtmp77 bool = 0 < y__9
            var jp109 string
            switch mtmp77 {
            case true:
                jp109 = "above"
            case false:
                jp109 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp107 = jp109
        }
        jp105 = jp107
    case 1:
        var jp111 string
        switch x76 {
        case 0:
            jp111 = "right"
        default:
            jp111 = "unknown"
        }
        jp105 = jp111
    default:
        jp105 = "unknown"
    }
    retv103 = jp105
    return retv103
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t113 string = point_type(p0__10)
    println__T_string(t113)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t114 string = line_to_string(line__12)
    println__T_string(t114)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int32_to_string(self__6)
    retv117 = t118
    return retv117
}

func println__T_string(value__1 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv123 string
    retv123 = self__38
    return retv123
}

func main() {
    main0()
}
