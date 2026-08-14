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
    color Color
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

type LineList interface {
    isLineList()
}

type Nil struct {}

func (_ Nil) isLineList() {}

type Cons struct {
    _0 Line
    _1 LineList
}

func (_ Cons) isLineList() {}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var x409 int32 = self__1.x
    var x410 int32 = self__1.y
    var x411 Color = self__1.color
    var t425 string = "Point { " + "x: "
    var t426 string
    var inline481 string = _goml_runtime_core_int32_to_string(x409)
    t426 = inline481
    var t427 string = t425 + t426
    var t428 string = t427 + ", "
    var t429 string = t428 + "y: "
    var t430 string
    var inline479 string = _goml_runtime_core_int32_to_string(x410)
    t430 = inline479
    var t431 string = t429 + t430
    var t432 string = t431 + ", "
    var t433 string = t432 + "color: "
    var t434 string
    switch x411 {
    case Red:
        t434 = "Color::Red"
    case Green:
        t434 = "Color::Green"
    case Blue:
        t434 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t435 string = t433 + t434
    var t436 string = t435 + " }"
    return t436
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x413 Point = self__8.from
    var x414 Point = self__8.to
    var x415 Color = self__8.color
    var t442 string = "Line { " + "from: "
    var t443 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x413)
    var t444 string = t442 + t443
    var t445 string = t444 + ", "
    var t446 string = t445 + "to: "
    var t447 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x414)
    var t448 string = t446 + t447
    var t449 string = t448 + ", "
    var t450 string = t449 + "color: "
    var t451 string
    switch x415 {
    case Red:
        t451 = "Color::Red"
    case Green:
        t451 = "Color::Green"
    case Blue:
        t451 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t452 string = t450 + t451
    var t453 string = t452 + " }"
    return t453
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x416 Line = self__15.(Cons)._0
        var x417 LineList = self__15.(Cons)._1
        var t461 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x416)
        var t462 string = "LineList::Cons(" + t461
        var t463 string = t462 + ", "
        var t464 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x417)
        var t465 string = t463 + t464
        var t466 string = t465 + ")"
        return t466
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline493 int32 = 10
    var inline494 int32 = 20
    var inline495 Point = Point{
        x: inline493,
        y: inline494,
        color: Red,
    }
    from__18 = inline495
    var to__19 Point
    var inline489 int32 = 30
    var inline490 int32 = 40
    var inline491 Point = Point{
        x: inline489,
        y: inline490,
        color: Green,
    }
    to__19 = inline491
    var line__20 Line
    var inline487 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline487
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t468 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline484)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
