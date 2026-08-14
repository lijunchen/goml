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
    var x186 Point = l__4.from
    var x187 Point = l__4.to
    var x188 Color = l__4.color
    var t207 string
    var inline257 int32 = x186.x
    var inline258 int32 = x186.y
    var inline261 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline257)
    var inline262 string = "Point { x: " + inline261
    var inline263 string = inline262 + ", y: "
    var inline264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline258)
    var inline265 string = inline263 + inline264
    var inline266 string = inline265 + " }"
    t207 = inline266
    var t208 string = "Line { from: " + t207
    var t209 string = t208 + ", to: "
    var t210 string
    var inline245 int32 = x187.x
    var inline246 int32 = x187.y
    var inline249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
    var inline250 string = "Point { x: " + inline249
    var inline251 string = inline250 + ", y: "
    var inline252 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline246)
    var inline253 string = inline251 + inline252
    var inline254 string = inline253 + " }"
    t210 = inline254
    var t211 string = t209 + t210
    var t212 string = t211 + ", color: "
    var t213 string
    switch x188 {
    case Red:
        t213 = "Red"
    case Green:
        t213 = "Green"
    case Blue:
        t213 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t214 string = t212 + t213
    var t215 string = t214 + " }"
    return t215
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t227 string
    var inline274 int32 = 0
    var inline275 int32 = 0
    switch inline274 {
    case 0:
        switch inline275 {
        case 0:
            t227 = "origin"
        case 1:
            t227 = "up"
        default:
            var inline277 bool = 0 < inline275
            switch inline277 {
            case true:
                t227 = "above"
            case false:
                t227 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline275 {
        case 0:
            t227 = "right"
        default:
            t227 = "unknown"
        }
    default:
        t227 = "unknown"
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline271)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t228 string = line_to_string(line__12)
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline268)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__33)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
