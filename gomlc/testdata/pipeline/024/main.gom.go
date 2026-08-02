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

func line_to_string(l__4 Line) string {
    var x159 Point = l__4.from
    var x160 Point = l__4.to
    var x161 Color = l__4.color
    var t180 string
    var inline230 int32 = x159.x
    var inline231 int32 = x159.y
    var inline234 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline230)
    var inline235 string = "Point { x: " + inline234
    var inline236 string = inline235 + ", y: "
    var inline237 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
    var inline238 string = inline236 + inline237
    var inline239 string = inline238 + " }"
    t180 = inline239
    var t181 string = "Line { from: " + t180
    var t182 string = t181 + ", to: "
    var t183 string
    var inline218 int32 = x160.x
    var inline219 int32 = x160.y
    var inline222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline218)
    var inline223 string = "Point { x: " + inline222
    var inline224 string = inline223 + ", y: "
    var inline225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
    var inline226 string = inline224 + inline225
    var inline227 string = inline226 + " }"
    t183 = inline227
    var t184 string = t182 + t183
    var t185 string = t184 + ", color: "
    var t186 string
    switch x161 {
    case Red:
        t186 = "Red"
    case Green:
        t186 = "Green"
    case Blue:
        t186 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t187 string = t185 + t186
    var t188 string = t187 + " }"
    return t188
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t200 string
    var inline247 int32 = 0
    var inline248 int32 = 0
    switch inline247 {
    case 0:
        switch inline248 {
        case 0:
            t200 = "origin"
        case 1:
            t200 = "up"
        default:
            var inline250 bool = 0 < inline248
            switch inline250 {
            case true:
                t200 = "above"
            case false:
                t200 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline248 {
        case 0:
            t200 = "right"
        default:
            t200 = "unknown"
        }
    default:
        t200 = "unknown"
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline244)
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
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t205 string = _goml_runtime_core_int32_to_string(self__6)
    return t205
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
