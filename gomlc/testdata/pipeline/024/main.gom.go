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
    var x181 Point = l__4.from
    var x182 Point = l__4.to
    var x183 Color = l__4.color
    var t202 string
    var inline252 int32 = x181.x
    var inline253 int32 = x181.y
    var inline256 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline252)
    var inline257 string = "Point { x: " + inline256
    var inline258 string = inline257 + ", y: "
    var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
    var inline260 string = inline258 + inline259
    var inline261 string = inline260 + " }"
    t202 = inline261
    var t203 string = "Line { from: " + t202
    var t204 string = t203 + ", to: "
    var t205 string
    var inline240 int32 = x182.x
    var inline241 int32 = x182.y
    var inline244 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline240)
    var inline245 string = "Point { x: " + inline244
    var inline246 string = inline245 + ", y: "
    var inline247 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
    var inline248 string = inline246 + inline247
    var inline249 string = inline248 + " }"
    t205 = inline249
    var t206 string = t204 + t205
    var t207 string = t206 + ", color: "
    var t208 string
    switch x183 {
    case Red:
        t208 = "Red"
    case Green:
        t208 = "Green"
    case Blue:
        t208 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t209 string = t207 + t208
    var t210 string = t209 + " }"
    return t210
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t222 string
    var inline269 int32 = 0
    var inline270 int32 = 0
    switch inline269 {
    case 0:
        switch inline270 {
        case 0:
            t222 = "origin"
        case 1:
            t222 = "up"
        default:
            var inline272 bool = 0 < inline270
            switch inline272 {
            case true:
                t222 = "above"
            case false:
                t222 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline270 {
        case 0:
            t222 = "right"
        default:
            t222 = "unknown"
        }
    default:
        t222 = "unknown"
    }
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline266)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t223 string = line_to_string(line__12)
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline263)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t227 string = _goml_runtime_core_int32_to_string(self__35)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
