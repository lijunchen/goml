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

type Wrapper__int32 struct {
    value int32
}

type Ordering int32

func make_point() Point {
    var t438 Point = Point{
        x: 0,
        y: 0,
    }
    return t438
}

func flip(point__0 Point) Point {
    var x409 int32 = point__0.x
    var x410 int32 = point__0.y
    var t441 Point = Point{
        x: x410,
        y: x409,
    }
    return t441
}

func x_add_1(p__4 Point) Point {
    var x412 int32 = p__4.x
    var x413 int32 = p__4.y
    var t447 int32 = x412 + 1
    var t448 Point = Point{
        x: t447,
        y: x413,
    }
    return t448
}

func point32_to_string(p__13 Point) string {
    var x421 int32 = p__13.x
    var x422 int32 = p__13.y
    var t455 string
    var inline503 string = _goml_runtime_core_int32_to_string(x421)
    t455 = inline503
    var t456 string = "Point { x: " + t455
    var t457 string = t456 + ", y: "
    var t458 string
    var inline501 string = _goml_runtime_core_int32_to_string(x422)
    t458 = inline501
    var t459 string = t457 + t458
    var t460 string = t459 + "}"
    return t460
}

func point32_to_string2(p__16 Point) string {
    var x424 int32 = p__16.x
    var x425 int32 = p__16.y
    var t463 string
    var inline507 string = _goml_runtime_core_int32_to_string(x424)
    t463 = inline507
    var t464 string = "Point { x: " + t463
    var t465 string = t464 + ", y: "
    var t466 string
    var inline505 string = _goml_runtime_core_int32_to_string(x425)
    t466 = inline505
    var t467 string = t465 + t466
    var t468 string = t467 + "}"
    return t468
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t486 string = point32_to_string(start__25)
    println__T_string(t486)
    var t487 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t487)
    var t488 string = point32_to_string2(swapped__26)
    println__T_string(t488)
    var a__29 Point = x_add_1(start__25)
    var t489 string
    var inline551 int32 = a__29.x
    var inline552 int32 = a__29.y
    var inline555 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline551)
    var inline556 string = "Point { x: " + inline555
    var inline557 string = inline556 + ", y: "
    var inline558 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline552)
    var inline559 string = inline557 + inline558
    var inline560 string = inline559 + "}"
    t489 = inline560
    var inline547 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t489)
    _goml_runtime_core_string_println(inline547)
    var t490 Point
    var inline540 int32 = start__25.x
    var inline541 int32 = start__25.y
    var inline544 int32 = inline540 + 1
    var inline545 Point = Point{
        x: inline544,
        y: inline541,
    }
    t490 = inline545
    var a__30 Point
    var inline533 int32 = t490.x
    var inline534 int32 = t490.y
    var inline537 Point = Point{
        x: inline534,
        y: inline533,
    }
    a__30 = inline537
    var t491 string
    var inline521 int32 = a__30.x
    var inline522 int32 = a__30.y
    var inline525 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline521)
    var inline526 string = "Point { x: " + inline525
    var inline527 string = inline526 + ", y: "
    var inline528 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline522)
    var inline529 string = inline527 + inline528
    var inline530 string = inline529 + "}"
    t491 = inline530
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t491)
    _goml_runtime_core_string_println(inline517)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t494 string = _goml_runtime_core_int32_to_string(self__33)
    return t494
}

func println__T_string(value__1 string) struct{} {
    var t496 string
    t496 = value__1
    _goml_runtime_core_string_println(t496)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
