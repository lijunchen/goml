package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Point struct {
    x int32
    y int32
}

type Wrapper__int32 struct {
    value int32
}

type Wrapper__unit struct {
    value struct{}
}

type Shape__int32 interface {
    isShape__int32()
}

type Shape__int32_Dot struct {
    _0 Point
}

func (_ Shape__int32_Dot) isShape__int32() {}

type Shape__int32_Wrapped struct {
    _0 Wrapper__int32
}

func (_ Shape__int32_Wrapped) isShape__int32() {}

type Shape__int32_Origin struct {}

func (_ Shape__int32_Origin) isShape__int32() {}

type Shape__unit interface {
    isShape__unit()
}

type Shape__unit_Dot struct {
    _0 Point
}

func (_ Shape__unit_Dot) isShape__unit() {}

type Shape__unit_Wrapped struct {
    _0 Wrapper__unit
}

func (_ Shape__unit_Wrapped) isShape__unit() {}

type Shape__unit_Origin struct {}

func (_ Shape__unit_Origin) isShape__unit() {}

func bounce_int(shape__0 Shape__int32) Shape__int32 {
    var jp26 Shape__int32
    var x0 Point
    var point__1 Point
    var t27 Shape__int32
    var x1 Wrapper__int32
    var inner__2 Wrapper__int32
    var t28 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        goto b2
    case Shape__int32_Wrapped:
        goto b3
    case Shape__int32_Origin:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp26
    b2:
    x0 = shape__0.(Shape__int32_Dot)._0
    point__1 = x0
    t27 = Shape__int32_Dot{
        _0: point__1,
    }
    jp26 = t27
    goto b1
    b3:
    x1 = shape__0.(Shape__int32_Wrapped)._0
    inner__2 = x1
    t28 = Shape__int32_Wrapped{
        _0: inner__2,
    }
    jp26 = t28
    goto b1
    b4:
    jp26 = Shape__int32_Origin{}
    goto b1
}

func point32_to_string(point__8 Point) string {
    var mtmp4 Point
    var x5 int32
    var x6 int32
    var y__10 int32
    var x__9 int32
    var t34 string
    var with_x__11 string
    var with_y_label__12 string
    var t35 string
    var with_y__13 string
    var t36 string
    mtmp4 = point__8
    x5 = mtmp4.x
    x6 = mtmp4.y
    y__10 = x6
    x__9 = x5
    t34 = int32_to_string(x__9)
    with_x__11 = "Point { x: " + t34
    with_y_label__12 = with_x__11 + ", y: "
    t35 = int32_to_string(y__10)
    with_y__13 = with_y_label__12 + t35
    t36 = with_y__13 + " }"
    return t36
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var mtmp7 Wrapper__int32
    var x8 int32
    var value__15 int32
    var t37 string
    var prefix__16 string
    var t38 string
    mtmp7 = wrapper__14
    x8 = mtmp7.value
    value__15 = x8
    t37 = int32_to_string(value__15)
    prefix__16 = "Wrapper[int32] { value: " + t37
    t38 = prefix__16 + " }"
    return t38
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var mtmp9 Wrapper__unit
    var x10 struct{}
    var value__18 struct{}
    var t39 string
    var prefix__19 string
    var t40 string
    mtmp9 = wrapper__17
    x10 = mtmp9.value
    value__18 = x10
    t39 = unit_to_string(value__18)
    prefix__19 = "Wrapper[unit] { value: " + t39
    t40 = prefix__19 + " }"
    return t40
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var jp42 string
    var x11 Point
    var point__21 Point
    var t43 string
    var prefix__22 string
    var t44 string
    var x12 Wrapper__int32
    var wrapper__23 Wrapper__int32
    var t45 string
    var prefix__24 string
    var t46 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        goto b2
    case Shape__int32_Wrapped:
        goto b3
    case Shape__int32_Origin:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp42
    b2:
    x11 = shape__20.(Shape__int32_Dot)._0
    point__21 = x11
    t43 = point32_to_string(point__21)
    prefix__22 = "Shape::Dot(" + t43
    t44 = prefix__22 + ")"
    jp42 = t44
    goto b1
    b3:
    x12 = shape__20.(Shape__int32_Wrapped)._0
    wrapper__23 = x12
    t45 = wrapper_int32_to_string(wrapper__23)
    prefix__24 = "Shape::Wrapped(" + t45
    t46 = prefix__24 + ")"
    jp42 = t46
    goto b1
    b4:
    jp42 = "Shape::Origin"
    goto b1
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var jp48 string
    var x13 Point
    var point__26 Point
    var t49 string
    var prefix__27 string
    var t50 string
    var x14 Wrapper__unit
    var wrapper__28 Wrapper__unit
    var t51 string
    var prefix__29 string
    var t52 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        goto b2
    case Shape__unit_Wrapped:
        goto b3
    case Shape__unit_Origin:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp48
    b2:
    x13 = shape__25.(Shape__unit_Dot)._0
    point__26 = x13
    t49 = point32_to_string(point__26)
    prefix__27 = "Shape::Dot(" + t49
    t50 = prefix__27 + ")"
    jp48 = t50
    goto b1
    b3:
    x14 = shape__25.(Shape__unit_Wrapped)._0
    wrapper__28 = x14
    t51 = wrapper_unit_to_string(wrapper__28)
    prefix__29 = "Shape::Wrapped(" + t51
    t52 = prefix__29 + ")"
    jp48 = t52
    goto b1
    b4:
    jp48 = "Shape::Origin"
    goto b1
}

func main0() struct{} {
    var t53 Point
    var t54 string
    var t55 Wrapper__int32
    var t56 string
    var t57 Wrapper__unit
    var t58 string
    var bounced_origin__30 Shape__int32
    var t59 Point
    var t60 Shape__int32
    var t61 string
    var t62 Wrapper__int32
    var t63 Shape__int32
    var t64 string
    var t65 string
    var t66 Point
    var t67 Shape__unit
    var t68 string
    var t69 Wrapper__unit
    var t70 Shape__unit
    var t71 string
    var t72 string
    var t73 Shape__int32
    var t74 struct{}
    t53 = Point{
        x: 3,
        y: 4,
    }
    t54 = point32_to_string(t53)
    string_println(t54)
    t55 = Wrapper__int32{
        value: 7,
    }
    t56 = wrapper_int32_to_string(t55)
    string_println(t56)
    t57 = Wrapper__unit{
        value: struct{}{},
    }
    t58 = wrapper_unit_to_string(t57)
    string_println(t58)
    bounced_origin__30 = bounce_int(Shape__int32_Origin{})
    t59 = Point{
        x: 3,
        y: 4,
    }
    t60 = Shape__int32_Dot{
        _0: t59,
    }
    t61 = shape_int32_to_string(t60)
    string_println(t61)
    t62 = Wrapper__int32{
        value: 7,
    }
    t63 = Shape__int32_Wrapped{
        _0: t62,
    }
    t64 = shape_int32_to_string(t63)
    string_println(t64)
    t65 = shape_int32_to_string(bounced_origin__30)
    string_println(t65)
    t66 = Point{
        x: 3,
        y: 4,
    }
    t67 = Shape__unit_Dot{
        _0: t66,
    }
    t68 = shape_unit_to_string(t67)
    string_println(t68)
    t69 = Wrapper__unit{
        value: struct{}{},
    }
    t70 = Shape__unit_Wrapped{
        _0: t69,
    }
    t71 = shape_unit_to_string(t70)
    string_println(t71)
    t72 = shape_unit_to_string(Shape__unit_Origin{})
    string_println(t72)
    t73 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t73)
    t74 = string_println("struct enums!")
    return t74
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var jp76 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        goto b2
    case Shape__int32_Wrapped:
        goto b3
    case Shape__int32_Origin:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp76
    b2:
    jp76 = 1
    goto b1
    b3:
    jp76 = 2
    goto b1
    b4:
    jp76 = 0
    goto b1
}

func main() {
    main0()
}
