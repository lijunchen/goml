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
    var retv167 string
    var mtmp155 Point = p__0
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__2 int32 = x157
    var x__1 int32 = x156
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var t169 string = "Point { x: " + t168
    var t170 string = t169 + ", y: "
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t172 string = t170 + t171
    var t173 string = t172 + " }"
    retv167 = t173
    return retv167
}

func color_to_string(c__3 Color) string {
    var retv175 string
    var jp177 string
    switch c__3 {
    case Red:
        jp177 = "Red"
    case Green:
        jp177 = "Green"
    case Blue:
        jp177 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
}

func line_to_string(l__4 Line) string {
    var retv179 string
    var mtmp158 Line = l__4
    var x159 Point = mtmp158.from
    var x160 Point = mtmp158.to
    var x161 Color = mtmp158.color
    var color__7 Color = x161
    var to__6 Point = x160
    var from__5 Point = x159
    var t180 string = point32_to_string(from__5)
    var t181 string = "Line { from: " + t180
    var t182 string = t181 + ", to: "
    var t183 string = point32_to_string(to__6)
    var t184 string = t182 + t183
    var t185 string = t184 + ", color: "
    var t186 string = color_to_string(color__7)
    var t187 string = t185 + t186
    var t188 string = t187 + " }"
    retv179 = t188
    return retv179
}

func point_type(p__8 Point) string {
    var retv190 string
    var x162 int32 = p__8.x
    var x163 int32 = p__8.y
    var jp192 string
    switch x162 {
    case 0:
        var jp194 string
        switch x163 {
        case 0:
            jp194 = "origin"
        case 1:
            jp194 = "up"
        default:
            var y__9 int32 = x163
            var mtmp164 bool = 0 < y__9
            var jp196 string
            switch mtmp164 {
            case true:
                jp196 = "above"
            case false:
                jp196 = "below"
            default:
                panic("non-exhaustive match")
            }
            jp194 = jp196
        }
        jp192 = jp194
    case 1:
        var jp198 string
        switch x163 {
        case 0:
            jp198 = "right"
        default:
            jp198 = "unknown"
        }
        jp192 = jp198
    default:
        jp192 = "unknown"
    }
    retv190 = jp192
    return retv190
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t200 string = point_type(p0__10)
    println__T_string(t200)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t201 string = line_to_string(line__12)
    println__T_string(t201)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv204 string
    var t205 string = _goml_runtime_core_int32_to_string(self__6)
    retv204 = t205
    return retv204
}

func println__T_string(value__1 string) struct{} {
    var t207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv210 string
    retv210 = self__38
    return retv210
}

func main() {
    main0()
}
