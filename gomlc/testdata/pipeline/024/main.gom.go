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
    var x415 Point = l__4.from
    var x416 Point = l__4.to
    var x417 Color = l__4.color
    var t436 string
    var inline486 int32 = x415.x
    var inline487 int32 = x415.y
    var inline490 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline486)
    var inline491 string = "Point { x: " + inline490
    var inline492 string = inline491 + ", y: "
    var inline493 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline487)
    var inline494 string = inline492 + inline493
    var inline495 string = inline494 + " }"
    t436 = inline495
    var t437 string = "Line { from: " + t436
    var t438 string = t437 + ", to: "
    var t439 string
    var inline474 int32 = x416.x
    var inline475 int32 = x416.y
    var inline478 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline474)
    var inline479 string = "Point { x: " + inline478
    var inline480 string = inline479 + ", y: "
    var inline481 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline475)
    var inline482 string = inline480 + inline481
    var inline483 string = inline482 + " }"
    t439 = inline483
    var t440 string = t438 + t439
    var t441 string = t440 + ", color: "
    var t442 string
    switch x417 {
    case Red:
        t442 = "Red"
    case Green:
        t442 = "Green"
    case Blue:
        t442 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t443 string = t441 + t442
    var t444 string = t443 + " }"
    return t444
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t456 string
    var inline503 int32 = 0
    var inline504 int32 = 0
    switch inline503 {
    case 0:
        switch inline504 {
        case 0:
            t456 = "origin"
        case 1:
            t456 = "up"
        default:
            var inline506 bool = 0 < inline504
            switch inline506 {
            case true:
                t456 = "above"
            case false:
                t456 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline504 {
        case 0:
            t456 = "right"
        default:
            t456 = "unknown"
        }
    default:
        t456 = "unknown"
    }
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t456)
    _goml_runtime_core_string_println(inline500)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t457 string = line_to_string(line__12)
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline497)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t461 string = _goml_runtime_core_int32_to_string(self__33)
    return t461
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
