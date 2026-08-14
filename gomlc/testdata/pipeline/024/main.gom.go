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
    var x191 Point = l__4.from
    var x192 Point = l__4.to
    var x193 Color = l__4.color
    var t212 string
    var inline262 int32 = x191.x
    var inline263 int32 = x191.y
    var inline266 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline262)
    var inline267 string = "Point { x: " + inline266
    var inline268 string = inline267 + ", y: "
    var inline269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline263)
    var inline270 string = inline268 + inline269
    var inline271 string = inline270 + " }"
    t212 = inline271
    var t213 string = "Line { from: " + t212
    var t214 string = t213 + ", to: "
    var t215 string
    var inline250 int32 = x192.x
    var inline251 int32 = x192.y
    var inline254 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline250)
    var inline255 string = "Point { x: " + inline254
    var inline256 string = inline255 + ", y: "
    var inline257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline251)
    var inline258 string = inline256 + inline257
    var inline259 string = inline258 + " }"
    t215 = inline259
    var t216 string = t214 + t215
    var t217 string = t216 + ", color: "
    var t218 string
    switch x193 {
    case Red:
        t218 = "Red"
    case Green:
        t218 = "Green"
    case Blue:
        t218 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t219 string = t217 + t218
    var t220 string = t219 + " }"
    return t220
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t232 string
    var inline279 int32 = 0
    var inline280 int32 = 0
    switch inline279 {
    case 0:
        switch inline280 {
        case 0:
            t232 = "origin"
        case 1:
            t232 = "up"
        default:
            var inline282 bool = 0 < inline280
            switch inline282 {
            case true:
                t232 = "above"
            case false:
                t232 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline280 {
        case 0:
            t232 = "right"
        default:
            t232 = "unknown"
        }
    default:
        t232 = "unknown"
    }
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline276)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t233 string = line_to_string(line__12)
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t237 string = _goml_runtime_core_int32_to_string(self__33)
    return t237
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
