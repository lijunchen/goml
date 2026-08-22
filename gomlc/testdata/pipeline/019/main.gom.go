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

type Wrapper__i32 struct {
    value int32
}

type Ordering int32

func make_point() Point {
    var t441 Point = Point{
        x: 0,
        y: 0,
    }
    return t441
}

func flip(point__0 Point) Point {
    var x412 int32 = point__0.x
    var x413 int32 = point__0.y
    var t444 Point = Point{
        x: x413,
        y: x412,
    }
    return t444
}

func x_add_1(p__4 Point) Point {
    var x415 int32 = p__4.x
    var x416 int32 = p__4.y
    var t450 int32 = x415 + 1
    var t451 Point = Point{
        x: t450,
        y: x416,
    }
    return t451
}

func point32_to_string(p__13 Point) string {
    var x424 int32 = p__13.x
    var x425 int32 = p__13.y
    var t458 string
    var inline506 string = _goml_runtime_core_int32_to_string(x424)
    t458 = inline506
    var t459 string = "Point { x: " + t458
    var t460 string = t459 + ", y: "
    var t461 string
    var inline504 string = _goml_runtime_core_int32_to_string(x425)
    t461 = inline504
    var t462 string = t460 + t461
    var t463 string = t462 + "}"
    return t463
}

func point32_to_string2(p__16 Point) string {
    var x427 int32 = p__16.x
    var x428 int32 = p__16.y
    var t466 string
    var inline510 string = _goml_runtime_core_int32_to_string(x427)
    t466 = inline510
    var t467 string = "Point { x: " + t466
    var t468 string = t467 + ", y: "
    var t469 string
    var inline508 string = _goml_runtime_core_int32_to_string(x428)
    t469 = inline508
    var t470 string = t468 + t469
    var t471 string = t470 + "}"
    return t471
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t489 string = point32_to_string(start__25)
    println__T_string(t489)
    var t490 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t490)
    var t491 string = point32_to_string2(swapped__26)
    println__T_string(t491)
    var a__29 Point = x_add_1(start__25)
    var t492 string
    var inline554 int32 = a__29.x
    var inline555 int32 = a__29.y
    var inline558 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline554)
    var inline559 string = "Point { x: " + inline558
    var inline560 string = inline559 + ", y: "
    var inline561 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline555)
    var inline562 string = inline560 + inline561
    var inline563 string = inline562 + "}"
    t492 = inline563
    var inline550 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t492)
    _goml_runtime_core_string_println(inline550)
    var t493 Point
    var inline543 int32 = start__25.x
    var inline544 int32 = start__25.y
    var inline547 int32 = inline543 + 1
    var inline548 Point = Point{
        x: inline547,
        y: inline544,
    }
    t493 = inline548
    var a__30 Point
    var inline536 int32 = t493.x
    var inline537 int32 = t493.y
    var inline540 Point = Point{
        x: inline537,
        y: inline536,
    }
    a__30 = inline540
    var t494 string
    var inline524 int32 = a__30.x
    var inline525 int32 = a__30.y
    var inline528 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline524)
    var inline529 string = "Point { x: " + inline528
    var inline530 string = inline529 + ", y: "
    var inline531 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline525)
    var inline532 string = inline530 + inline531
    var inline533 string = inline532 + "}"
    t494 = inline533
    var inline520 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t494)
    _goml_runtime_core_string_println(inline520)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t497 string = _goml_runtime_core_int32_to_string(self__33)
    return t497
}

func println__T_string(value__1 string) struct{} {
    var t499 string
    t499 = value__1
    _goml_runtime_core_string_println(t499)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
