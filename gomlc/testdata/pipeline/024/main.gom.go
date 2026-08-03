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
    var x140 Point = l__4.from
    var x141 Point = l__4.to
    var x142 Color = l__4.color
    var t161 string
    var inline211 int32 = x140.x
    var inline212 int32 = x140.y
    var inline215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline211)
    var inline216 string = "Point { x: " + inline215
    var inline217 string = inline216 + ", y: "
    var inline218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline212)
    var inline219 string = inline217 + inline218
    var inline220 string = inline219 + " }"
    t161 = inline220
    var t162 string = "Line { from: " + t161
    var t163 string = t162 + ", to: "
    var t164 string
    var inline199 int32 = x141.x
    var inline200 int32 = x141.y
    var inline203 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline199)
    var inline204 string = "Point { x: " + inline203
    var inline205 string = inline204 + ", y: "
    var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline200)
    var inline207 string = inline205 + inline206
    var inline208 string = inline207 + " }"
    t164 = inline208
    var t165 string = t163 + t164
    var t166 string = t165 + ", color: "
    var t167 string
    switch x142 {
    case Red:
        t167 = "Red"
    case Green:
        t167 = "Green"
    case Blue:
        t167 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t168 string = t166 + t167
    var t169 string = t168 + " }"
    return t169
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t181 string
    var inline228 int32 = 0
    var inline229 int32 = 0
    switch inline228 {
    case 0:
        switch inline229 {
        case 0:
            t181 = "origin"
        case 1:
            t181 = "up"
        default:
            var inline231 bool = 0 < inline229
            switch inline231 {
            case true:
                t181 = "above"
            case false:
                t181 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline229 {
        case 0:
            t181 = "right"
        default:
            t181 = "unknown"
        }
    default:
        t181 = "unknown"
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline225)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t182 string = line_to_string(line__12)
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t186 string = _goml_runtime_core_int32_to_string(self__35)
    return t186
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
