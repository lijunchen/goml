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

type Ordering int32

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func line_to_string(l__4 Line) string {
    var x412 Point = l__4.from
    var x413 Point = l__4.to
    var x414 Color = l__4.color
    var t433 string
    var inline483 int32 = x412.x
    var inline484 int32 = x412.y
    var inline487 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline483)
    var inline488 string = "Point { x: " + inline487
    var inline489 string = inline488 + ", y: "
    var inline490 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline484)
    var inline491 string = inline489 + inline490
    var inline492 string = inline491 + " }"
    t433 = inline492
    var t434 string = "Line { from: " + t433
    var t435 string = t434 + ", to: "
    var t436 string
    var inline471 int32 = x413.x
    var inline472 int32 = x413.y
    var inline475 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline471)
    var inline476 string = "Point { x: " + inline475
    var inline477 string = inline476 + ", y: "
    var inline478 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline472)
    var inline479 string = inline477 + inline478
    var inline480 string = inline479 + " }"
    t436 = inline480
    var t437 string = t435 + t436
    var t438 string = t437 + ", color: "
    var t439 string
    switch x414 {
    case Red:
        t439 = "Red"
    case Green:
        t439 = "Green"
    case Blue:
        t439 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t440 string = t438 + t439
    var t441 string = t440 + " }"
    return t441
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t453 string
    var inline500 int32 = 0
    var inline501 int32 = 0
    switch inline500 {
    case 0:
        switch inline501 {
        case 0:
            t453 = "origin"
        case 1:
            t453 = "up"
        default:
            var inline503 bool = 0 < inline501
            switch inline503 {
            case true:
                t453 = "above"
            case false:
                t453 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline501 {
        case 0:
            t453 = "right"
        default:
            t453 = "unknown"
        }
    default:
        t453 = "unknown"
    }
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t453)
    _goml_runtime_core_string_println(inline497)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t454 string = line_to_string(line__12)
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t454)
    _goml_runtime_core_string_println(inline494)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t458 string = _goml_runtime_core_int32_to_string(self__33)
    return t458
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
