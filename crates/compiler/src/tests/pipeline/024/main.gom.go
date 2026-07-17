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
    var retv70 string
    var mtmp58 Point = p__0
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var y__2 int32 = x60
    var x__1 int32 = x59
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t72 string = "Point { x: " + t71
    var t73 string = t72 + ", y: "
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t75 string = t73 + t74
    var t76 string = t75 + " }"
    retv70 = t76
    return retv70
}

func color_to_string(c__3 Color) string {
    var retv78 string
    var jp80 string
    switch c__3 {
    case Red:
        jp80 = "Red"
    case Green:
        jp80 = "Green"
    case Blue:
        jp80 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv78 = jp80
    return retv78
}

func line_to_string(l__4 Line) string {
    var retv82 string
    var mtmp61 Line = l__4
    var x62 Point = mtmp61.from
    var x63 Point = mtmp61.to
    var x64 Color = mtmp61.color
    var color__7 Color = x64
    var to__6 Point = x63
    var from__5 Point = x62
    var t83 string = point32_to_string(from__5)
    var t84 string = "Line { from: " + t83
    var t85 string = t84 + ", to: "
    var t86 string = point32_to_string(to__6)
    var t87 string = t85 + t86
    var t88 string = t87 + ", color: "
    var t89 string = color_to_string(color__7)
    var t90 string = t88 + t89
    var t91 string = t90 + " }"
    retv82 = t91
    return retv82
}

func point_type(p__8 Point) string {
    var retv93 string
    var x65 int32 = p__8.x
    var x66 int32 = p__8.y
    var jp95 string
    switch x65 {
    case 0:
        var jp97 string
        switch x66 {
        case 0:
            jp97 = "origin"
        case 1:
            jp97 = "up"
        default:
            var y__9 int32 = x66
            var mtmp67 bool = 0 < y__9
            var jp99 string
            switch mtmp67 {
            case true:
                jp99 = "above"
            case false:
                jp99 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp97 = jp99
        }
        jp95 = jp97
    case 1:
        var jp101 string
        switch x66 {
        case 0:
            jp101 = "right"
        default:
            jp101 = "unknown"
        }
        jp95 = jp101
    default:
        jp95 = "unknown"
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t103 string = point_type(p0__10)
    println__T_string(t103)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t104 string = line_to_string(line__12)
    println__T_string(t104)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__2)
    retv107 = t108
    return retv107
}

func println__T_string(value__1 string) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv113 string
    retv113 = self__34
    return retv113
}

func main() {
    main0()
}
