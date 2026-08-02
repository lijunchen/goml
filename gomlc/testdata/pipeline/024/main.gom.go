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
    var x156 int32 = p__0.x
    var x157 int32 = p__0.y
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x156)
    var t169 string = "Point { x: " + t168
    var t170 string = t169 + ", y: "
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
    var t172 string = t170 + t171
    var t173 string = t172 + " }"
    return t173
}

func color_to_string(c__3 Color) string {
    switch c__3 {
    case Red:
        return "Red"
    case Green:
        return "Green"
    case Blue:
        return "Blue"
    default:
        panic("non-exhaustive match")
    }
}

func line_to_string(l__4 Line) string {
    var x159 Point = l__4.from
    var x160 Point = l__4.to
    var x161 Color = l__4.color
    var t180 string = point32_to_string(x159)
    var t181 string = "Line { from: " + t180
    var t182 string = t181 + ", to: "
    var t183 string = point32_to_string(x160)
    var t184 string = t182 + t183
    var t185 string = t184 + ", color: "
    var t186 string = color_to_string(x161)
    var t187 string = t185 + t186
    var t188 string = t187 + " }"
    return t188
}

func point_type(p__8 Point) string {
    var x162 int32 = p__8.x
    var x163 int32 = p__8.y
    switch x162 {
    case 0:
        switch x163 {
        case 0:
            return "origin"
        case 1:
            return "up"
        default:
            var mtmp164 bool = 0 < x163
            switch mtmp164 {
            case true:
                return "above"
            case false:
                return "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch x163 {
        case 0:
            return "right"
        default:
            return "unknown"
        }
    default:
        return "unknown"
    }
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
    var t205 string = _goml_runtime_core_int32_to_string(self__6)
    return t205
}

func println__T_string(value__1 string) struct{} {
    var t207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
