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
    var retv73 string
    var mtmp61 Point = p__0
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__2 int32 = x63
    var x__1 int32 = x62
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t75 string = "Point { x: " + t74
    var t76 string = t75 + ", y: "
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t78 string = t76 + t77
    var t79 string = t78 + " }"
    retv73 = t79
    return retv73
}

func color_to_string(c__3 Color) string {
    var retv81 string
    var jp83 string
    switch c__3 {
    case Red:
        jp83 = "Red"
    case Green:
        jp83 = "Green"
    case Blue:
        jp83 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func line_to_string(l__4 Line) string {
    var retv85 string
    var mtmp64 Line = l__4
    var x65 Point = mtmp64.from
    var x66 Point = mtmp64.to
    var x67 Color = mtmp64.color
    var color__7 Color = x67
    var to__6 Point = x66
    var from__5 Point = x65
    var t86 string = point32_to_string(from__5)
    var t87 string = "Line { from: " + t86
    var t88 string = t87 + ", to: "
    var t89 string = point32_to_string(to__6)
    var t90 string = t88 + t89
    var t91 string = t90 + ", color: "
    var t92 string = color_to_string(color__7)
    var t93 string = t91 + t92
    var t94 string = t93 + " }"
    retv85 = t94
    return retv85
}

func point_type(p__8 Point) string {
    var retv96 string
    var x68 int32 = p__8.x
    var x69 int32 = p__8.y
    var jp98 string
    switch x68 {
    case 0:
        var jp100 string
        switch x69 {
        case 0:
            jp100 = "origin"
        case 1:
            jp100 = "up"
        default:
            var y__9 int32 = x69
            var mtmp70 bool = 0 < y__9
            var jp102 string
            switch mtmp70 {
            case true:
                jp102 = "above"
            case false:
                jp102 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp100 = jp102
        }
        jp98 = jp100
    case 1:
        var jp104 string
        switch x69 {
        case 0:
            jp104 = "right"
        default:
            jp104 = "unknown"
        }
        jp98 = jp104
    default:
        jp98 = "unknown"
    }
    retv96 = jp98
    return retv96
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t106 string = point_type(p0__10)
    println__T_string(t106)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t107 string = line_to_string(line__12)
    println__T_string(t107)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__5)
    retv110 = t111
    return retv110
}

func println__T_string(value__1 string) struct{} {
    var t113 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t113)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv116 string
    retv116 = self__37
    return retv116
}

func main() {
    main0()
}
