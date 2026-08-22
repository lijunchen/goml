package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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

type Wrapper__unit struct {
    value struct{}
}

type Ordering int32

type Shape__i32 struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__i32
}

type Shape__unit struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__unit
}

func bounce_int(shape__0 Shape__i32) Shape__i32 {
    switch shape__0._tag {
    case 0:
        var x411 Point = shape__0._v0_0
        var t440 Shape__i32 = Shape__i32{
            _tag: 0,
            _v0_0: x411,
        }
        return t440
    case 1:
        var x412 Wrapper__i32 = shape__0._v1_0
        var t441 Shape__i32 = Shape__i32{
            _tag: 1,
            _v1_0: x412,
        }
        return t441
    case 2:
        return Shape__i32{
            _tag: 2,
        }
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x416 int32 = point__8.x
    var x417 int32 = point__8.y
    var t453 string
    var inline521 string = _goml_runtime_core_int32_to_string(x416)
    t453 = inline521
    var with_x__11 string = "Point { x: " + t453
    var with_y_label__12 string = with_x__11 + ", y: "
    var t454 string
    var inline519 string = _goml_runtime_core_int32_to_string(x417)
    t454 = inline519
    var with_y__13 string = with_y_label__12 + t454
    var t455 string = with_y__13 + " }"
    return t455
}

func wrapper_int32_to_string(wrapper__14 Wrapper__i32) string {
    var x419 int32 = wrapper__14.value
    var t458 string
    var inline523 string = _goml_runtime_core_int32_to_string(x419)
    t458 = inline523
    var prefix__16 string = "Wrapper[i32] { value: " + t458
    var t459 string = prefix__16 + " }"
    return t459
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x421 struct{} = wrapper__17.value
    var t462 string
    var inline525 string = _goml_runtime_core_unit_to_string(x421)
    t462 = inline525
    var prefix__19 string = "Wrapper[unit] { value: " + t462
    var t463 string = prefix__19 + " }"
    return t463
}

func shape_int32_to_string(shape__20 Shape__i32) string {
    switch shape__20._tag {
    case 0:
        var x422 Point = shape__20._v0_0
        var t468 string
        var inline528 int32 = x422.x
        var inline529 int32 = x422.y
        var inline532 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline528)
        var inline533 string = "Point { x: " + inline532
        var inline534 string = inline533 + ", y: "
        var inline535 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline529)
        var inline536 string = inline534 + inline535
        var inline537 string = inline536 + " }"
        t468 = inline537
        var prefix__22 string = "Shape::Dot(" + t468
        var t469 string = prefix__22 + ")"
        return t469
    case 1:
        var x423 Wrapper__i32 = shape__20._v1_0
        var t470 string
        var inline540 int32 = x423.value
        var inline542 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline540)
        var inline543 string = "Wrapper[i32] { value: " + inline542
        var inline544 string = inline543 + " }"
        t470 = inline544
        var prefix__24 string = "Shape::Wrapped(" + t470
        var t471 string = prefix__24 + ")"
        return t471
    case 2:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25._tag {
    case 0:
        var x424 Point = shape__25._v0_0
        var t476 string
        var inline547 int32 = x424.x
        var inline548 int32 = x424.y
        var inline551 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline547)
        var inline552 string = "Point { x: " + inline551
        var inline553 string = inline552 + ", y: "
        var inline554 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline548)
        var inline555 string = inline553 + inline554
        var inline556 string = inline555 + " }"
        t476 = inline556
        var prefix__27 string = "Shape::Dot(" + t476
        var t477 string = prefix__27 + ")"
        return t477
    case 1:
        var x425 Wrapper__unit = shape__25._v1_0
        var t478 string
        var inline559 struct{} = x425.value
        var inline561 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline559)
        var inline562 string = "Wrapper[unit] { value: " + inline561
        var inline563 string = inline562 + " }"
        t478 = inline563
        var prefix__29 string = "Shape::Wrapped(" + t478
        var t479 string = prefix__29 + ")"
        return t479
    case 2:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t481 Point = Point{
        x: 3,
        y: 4,
    }
    var t482 string = point32_to_string(t481)
    println__T_string(t482)
    var t483 Wrapper__i32 = Wrapper__i32{
        value: 7,
    }
    var t484 string = wrapper_int32_to_string(t483)
    println__T_string(t484)
    var t485 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t486 string = wrapper_unit_to_string(t485)
    println__T_string(t486)
    var bounced_origin__30 Shape__i32 = bounce_int(Shape__i32{
        _tag: 2,
    })
    var t487 Point = Point{
        x: 3,
        y: 4,
    }
    var t488 Shape__i32 = Shape__i32{
        _tag: 0,
        _v0_0: t487,
    }
    var t489 string = shape_int32_to_string(t488)
    println__T_string(t489)
    var t490 Wrapper__i32 = Wrapper__i32{
        value: 7,
    }
    var t491 Shape__i32 = Shape__i32{
        _tag: 1,
        _v1_0: t490,
    }
    var t492 string = shape_int32_to_string(t491)
    var inline600 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t492)
    _goml_runtime_core_string_println(inline600)
    var t493 string = shape_int32_to_string(bounced_origin__30)
    var inline597 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t493)
    _goml_runtime_core_string_println(inline597)
    var t494 Point = Point{
        x: 3,
        y: 4,
    }
    var t495 Shape__unit = Shape__unit{
        _tag: 0,
        _v0_0: t494,
    }
    var t496 string = shape_unit_to_string(t495)
    var inline594 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t496)
    _goml_runtime_core_string_println(inline594)
    var t497 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t498 Shape__unit = Shape__unit{
        _tag: 1,
        _v1_0: t497,
    }
    var t499 string = shape_unit_to_string(t498)
    var inline591 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t499)
    _goml_runtime_core_string_println(inline591)
    var t500 string
    t500 = "Shape::Origin"
    var inline577 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t500)
    _goml_runtime_core_string_println(inline577)
    var t501 Shape__i32
    t501 = Shape__i32{
        _tag: 2,
    }
    switch t501._tag {
    case 0:
    case 1:
    case 2:
    default:
        panic("non-exhaustive match")
    }
    var inline565 string = "struct enums!"
    var inline566 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline565)
    _goml_runtime_core_string_println(inline566)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t505 string = _goml_runtime_core_int32_to_string(self__33)
    return t505
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t508 string = _goml_runtime_core_unit_to_string(self__147)
    return t508
}

func println__T_string(value__1 string) struct{} {
    var t510 string
    t510 = value__1
    _goml_runtime_core_string_println(t510)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
