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
    var retv164 string
    var mtmp152 Point = p__0
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var y__2 int32 = x154
    var x__1 int32 = x153
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t166 string = "Point { x: " + t165
    var t167 string = t166 + ", y: "
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t169 string = t167 + t168
    var t170 string = t169 + " }"
    retv164 = t170
    return retv164
}

func color_to_string(c__3 Color) string {
    var retv172 string
    var jp174 string
    switch c__3 {
    case Red:
        jp174 = "Red"
    case Green:
        jp174 = "Green"
    case Blue:
        jp174 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv172 = jp174
    return retv172
}

func line_to_string(l__4 Line) string {
    var retv176 string
    var mtmp155 Line = l__4
    var x156 Point = mtmp155.from
    var x157 Point = mtmp155.to
    var x158 Color = mtmp155.color
    var color__7 Color = x158
    var to__6 Point = x157
    var from__5 Point = x156
    var t177 string = point32_to_string(from__5)
    var t178 string = "Line { from: " + t177
    var t179 string = t178 + ", to: "
    var t180 string = point32_to_string(to__6)
    var t181 string = t179 + t180
    var t182 string = t181 + ", color: "
    var t183 string = color_to_string(color__7)
    var t184 string = t182 + t183
    var t185 string = t184 + " }"
    retv176 = t185
    return retv176
}

func point_type(p__8 Point) string {
    var retv187 string
    var x159 int32 = p__8.x
    var x160 int32 = p__8.y
    var jp189 string
    switch x159 {
    case 0:
        var jp191 string
        switch x160 {
        case 0:
            jp191 = "origin"
        case 1:
            jp191 = "up"
        default:
            var y__9 int32 = x160
            var mtmp161 bool = 0 < y__9
            var jp193 string
            switch mtmp161 {
            case true:
                jp193 = "above"
            case false:
                jp193 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp191 = jp193
        }
        jp189 = jp191
    case 1:
        var jp195 string
        switch x160 {
        case 0:
            jp195 = "right"
        default:
            jp195 = "unknown"
        }
        jp189 = jp195
    default:
        jp189 = "unknown"
    }
    retv187 = jp189
    return retv187
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t197 string = point_type(p0__10)
    println__T_string(t197)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t198 string = line_to_string(line__12)
    println__T_string(t198)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv201 string
    var t202 string = _goml_runtime_core_int32_to_string(self__6)
    retv201 = t202
    return retv201
}

func println__T_string(value__1 string) struct{} {
    var t204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv207 string
    retv207 = self__38
    return retv207
}

func main() {
    main0()
}
