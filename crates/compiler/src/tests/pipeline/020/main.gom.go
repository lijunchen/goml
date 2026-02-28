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
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x0 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x0
        var t27 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp26 = t27
    case Shape__int32_Wrapped:
        var x1 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x1
        var t28 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp26 = t28
    case Shape__int32_Origin:
        jp26 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    return jp26
}

func point32_to_string(point__8 Point) string {
    var mtmp4 Point = point__8
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__10 int32 = x6
    var x__9 int32 = x5
    var t34 string = int32_to_string(x__9)
    var with_x__11 string = "Point { x: " + t34
    var with_y_label__12 string = with_x__11 + ", y: "
    var t35 string = int32_to_string(y__10)
    var with_y__13 string = with_y_label__12 + t35
    var t36 string = with_y__13 + " }"
    return t36
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var mtmp7 Wrapper__int32 = wrapper__14
    var x8 int32 = mtmp7.value
    var value__15 int32 = x8
    var t37 string = int32_to_string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t37
    var t38 string = prefix__16 + " }"
    return t38
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var mtmp9 Wrapper__unit = wrapper__17
    var x10 struct{} = mtmp9.value
    var value__18 struct{} = x10
    var t39 string = unit_to_string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t39
    var t40 string = prefix__19 + " }"
    return t40
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var jp42 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x11 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x11
        var t43 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t43
        var t44 string = prefix__22 + ")"
        jp42 = t44
    case Shape__int32_Wrapped:
        var x12 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x12
        var t45 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t45
        var t46 string = prefix__24 + ")"
        jp42 = t46
    case Shape__int32_Origin:
        jp42 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    return jp42
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var jp48 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x13 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x13
        var t49 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t49
        var t50 string = prefix__27 + ")"
        jp48 = t50
    case Shape__unit_Wrapped:
        var x14 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x14
        var t51 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t51
        var t52 string = prefix__29 + ")"
        jp48 = t52
    case Shape__unit_Origin:
        jp48 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    return jp48
}

func main0() struct{} {
    var t53 Point = Point{
        x: 3,
        y: 4,
    }
    var t54 string = point32_to_string(t53)
    string_println(t54)
    var t55 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t56 string = wrapper_int32_to_string(t55)
    string_println(t56)
    var t57 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t58 string = wrapper_unit_to_string(t57)
    string_println(t58)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t59 Point = Point{
        x: 3,
        y: 4,
    }
    var t60 Shape__int32 = Shape__int32_Dot{
        _0: t59,
    }
    var t61 string = shape_int32_to_string(t60)
    string_println(t61)
    var t62 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t63 Shape__int32 = Shape__int32_Wrapped{
        _0: t62,
    }
    var t64 string = shape_int32_to_string(t63)
    string_println(t64)
    var t65 string = shape_int32_to_string(bounced_origin__30)
    string_println(t65)
    var t66 Point = Point{
        x: 3,
        y: 4,
    }
    var t67 Shape__unit = Shape__unit_Dot{
        _0: t66,
    }
    var t68 string = shape_unit_to_string(t67)
    string_println(t68)
    var t69 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t70 Shape__unit = Shape__unit_Wrapped{
        _0: t69,
    }
    var t71 string = shape_unit_to_string(t70)
    string_println(t71)
    var t72 string = shape_unit_to_string(Shape__unit_Origin{})
    string_println(t72)
    var t73 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t73)
    var t74 struct{} = string_println("struct enums!")
    return t74
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var jp76 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp76 = 1
    case Shape__int32_Wrapped:
        jp76 = 2
    case Shape__int32_Origin:
        jp76 = 0
    default:
        panic("non-exhaustive match")
    }
    return jp76
}

func main() {
    main0()
}
