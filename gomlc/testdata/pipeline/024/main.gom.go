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
    var x176 Point = l__4.from
    var x177 Point = l__4.to
    var x178 Color = l__4.color
    var t197 string
    var inline247 int32 = x176.x
    var inline248 int32 = x176.y
    var inline251 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline247)
    var inline252 string = "Point { x: " + inline251
    var inline253 string = inline252 + ", y: "
    var inline254 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
    var inline255 string = inline253 + inline254
    var inline256 string = inline255 + " }"
    t197 = inline256
    var t198 string = "Line { from: " + t197
    var t199 string = t198 + ", to: "
    var t200 string
    var inline235 int32 = x177.x
    var inline236 int32 = x177.y
    var inline239 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline235)
    var inline240 string = "Point { x: " + inline239
    var inline241 string = inline240 + ", y: "
    var inline242 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
    var inline243 string = inline241 + inline242
    var inline244 string = inline243 + " }"
    t200 = inline244
    var t201 string = t199 + t200
    var t202 string = t201 + ", color: "
    var t203 string
    switch x178 {
    case Red:
        t203 = "Red"
    case Green:
        t203 = "Green"
    case Blue:
        t203 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t204 string = t202 + t203
    var t205 string = t204 + " }"
    return t205
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t217 string
    var inline264 int32 = 0
    var inline265 int32 = 0
    switch inline264 {
    case 0:
        switch inline265 {
        case 0:
            t217 = "origin"
        case 1:
            t217 = "up"
        default:
            var inline267 bool = 0 < inline265
            switch inline267 {
            case true:
                t217 = "above"
            case false:
                t217 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline265 {
        case 0:
            t217 = "right"
        default:
            t217 = "unknown"
        }
    default:
        t217 = "unknown"
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline261)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t218 string = line_to_string(line__12)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__33)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
