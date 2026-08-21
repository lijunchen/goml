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
    var x412 int32 = self__1.x
    var x413 int32 = self__1.y
    var x414 Color = self__1.color
    var t428 string = "Point { " + "x: "
    var t429 string
    var inline484 string = _goml_runtime_core_int32_to_string(x412)
    t429 = inline484
    var t430 string = t428 + t429
    var t431 string = t430 + ", "
    var t432 string = t431 + "y: "
    var t433 string
    var inline482 string = _goml_runtime_core_int32_to_string(x413)
    t433 = inline482
    var t434 string = t432 + t433
    var t435 string = t434 + ", "
    var t436 string = t435 + "color: "
    var t437 string
    switch x414 {
    case Red:
        t437 = "Color::Red"
    case Green:
        t437 = "Color::Green"
    case Blue:
        t437 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t438 string = t436 + t437
    var t439 string = t438 + " }"
    return t439
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x416 Point = self__8.from
    var x417 Point = self__8.to
    var x418 Color = self__8.color
    var t445 string = "Line { " + "from: "
    var t446 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x416)
    var t447 string = t445 + t446
    var t448 string = t447 + ", "
    var t449 string = t448 + "to: "
    var t450 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x417)
    var t451 string = t449 + t450
    var t452 string = t451 + ", "
    var t453 string = t452 + "color: "
    var t454 string
    switch x418 {
    case Red:
        t454 = "Color::Red"
    case Green:
        t454 = "Color::Green"
    case Blue:
        t454 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t455 string = t453 + t454
    var t456 string = t455 + " }"
    return t456
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x419 Line = self__15.(Cons)._0
        var x420 LineList = self__15.(Cons)._1
        var t464 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x419)
        var t465 string = "LineList::Cons(" + t464
        var t466 string = t465 + ", "
        var t467 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x420)
        var t468 string = t466 + t467
        var t469 string = t468 + ")"
        return t469
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline496 int32 = 10
    var inline497 int32 = 20
    var inline498 Point = Point{
        x: inline496,
        y: inline497,
        color: Red,
    }
    from__18 = inline498
    var to__19 Point
    var inline492 int32 = 30
    var inline493 int32 = 40
    var inline494 Point = Point{
        x: inline492,
        y: inline493,
        color: Green,
    }
    to__19 = inline494
    var line__20 Line
    var inline490 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline490
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t471 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline487)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
