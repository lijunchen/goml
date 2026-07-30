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
    var retv120 string
    var mtmp108 Point = p__0
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var y__2 int32 = x110
    var x__1 int32 = x109
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t122 string = "Point { x: " + t121
    var t123 string = t122 + ", y: "
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t125 string = t123 + t124
    var t126 string = t125 + " }"
    retv120 = t126
    return retv120
}

func color_to_string(c__3 Color) string {
    var retv128 string
    var jp130 string
    switch c__3 {
    case Red:
        jp130 = "Red"
    case Green:
        jp130 = "Green"
    case Blue:
        jp130 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv128 = jp130
    return retv128
}

func line_to_string(l__4 Line) string {
    var retv132 string
    var mtmp111 Line = l__4
    var x112 Point = mtmp111.from
    var x113 Point = mtmp111.to
    var x114 Color = mtmp111.color
    var color__7 Color = x114
    var to__6 Point = x113
    var from__5 Point = x112
    var t133 string = point32_to_string(from__5)
    var t134 string = "Line { from: " + t133
    var t135 string = t134 + ", to: "
    var t136 string = point32_to_string(to__6)
    var t137 string = t135 + t136
    var t138 string = t137 + ", color: "
    var t139 string = color_to_string(color__7)
    var t140 string = t138 + t139
    var t141 string = t140 + " }"
    retv132 = t141
    return retv132
}

func point_type(p__8 Point) string {
    var retv143 string
    var x115 int32 = p__8.x
    var x116 int32 = p__8.y
    var jp145 string
    switch x115 {
    case 0:
        var jp147 string
        switch x116 {
        case 0:
            jp147 = "origin"
        case 1:
            jp147 = "up"
        default:
            var y__9 int32 = x116
            var mtmp117 bool = 0 < y__9
            var jp149 string
            switch mtmp117 {
            case true:
                jp149 = "above"
            case false:
                jp149 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp147 = jp149
        }
        jp145 = jp147
    case 1:
        var jp151 string
        switch x116 {
        case 0:
            jp151 = "right"
        default:
            jp151 = "unknown"
        }
        jp145 = jp151
    default:
        jp145 = "unknown"
    }
    retv143 = jp145
    return retv143
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t153 string = point_type(p0__10)
    println__T_string(t153)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t154 string = line_to_string(line__12)
    println__T_string(t154)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv157 string
    var t158 string = _goml_runtime_core_int32_to_string(self__6)
    retv157 = t158
    return retv157
}

func println__T_string(value__1 string) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv163 string
    retv163 = self__38
    return retv163
}

func main() {
    main0()
}
