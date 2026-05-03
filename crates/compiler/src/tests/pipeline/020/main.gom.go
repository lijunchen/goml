package main

import (
    _goml_fmt "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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
    var retv26 Shape__int32
    var jp28 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x0 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x0
        var t29 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp28 = t29
    case Shape__int32_Wrapped:
        var x1 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x1
        var t30 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp28 = t30
    case Shape__int32_Origin:
        jp28 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func point32_to_string(point__8 Point) string {
    var retv41 string
    var mtmp4 Point = point__8
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__10 int32 = x6
    var x__9 int32 = x5
    var t42 string = int32_to_string(x__9)
    var with_x__11 string = "Point { x: " + t42
    var with_y_label__12 string = with_x__11 + ", y: "
    var t43 string = int32_to_string(y__10)
    var with_y__13 string = with_y_label__12 + t43
    var t44 string = with_y__13 + " }"
    retv41 = t44
    return retv41
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv46 string
    var mtmp7 Wrapper__int32 = wrapper__14
    var x8 int32 = mtmp7.value
    var value__15 int32 = x8
    var t47 string = int32_to_string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t47
    var t48 string = prefix__16 + " }"
    retv46 = t48
    return retv46
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv50 string
    var mtmp9 Wrapper__unit = wrapper__17
    var x10 struct{} = mtmp9.value
    var value__18 struct{} = x10
    var t51 string = unit_to_string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t51
    var t52 string = prefix__19 + " }"
    retv50 = t52
    return retv50
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv54 string
    var jp56 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x11 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x11
        var t57 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t57
        var t58 string = prefix__22 + ")"
        jp56 = t58
    case Shape__int32_Wrapped:
        var x12 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x12
        var t59 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t59
        var t60 string = prefix__24 + ")"
        jp56 = t60
    case Shape__int32_Origin:
        jp56 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv62 string
    var jp64 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x13 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x13
        var t65 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t65
        var t66 string = prefix__27 + ")"
        jp64 = t66
    case Shape__unit_Wrapped:
        var x14 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x14
        var t67 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t67
        var t68 string = prefix__29 + ")"
        jp64 = t68
    case Shape__unit_Origin:
        jp64 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv62 = jp64
    return retv62
}

func main0() struct{} {
    var t70 Point = Point{
        x: 3,
        y: 4,
    }
    var t71 string = point32_to_string(t70)
    println__T_string(t71)
    var t72 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t73 string = wrapper_int32_to_string(t72)
    println__T_string(t73)
    var t74 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t75 string = wrapper_unit_to_string(t74)
    println__T_string(t75)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t76 Point = Point{
        x: 3,
        y: 4,
    }
    var t77 Shape__int32 = Shape__int32_Dot{
        _0: t76,
    }
    var t78 string = shape_int32_to_string(t77)
    println__T_string(t78)
    var t79 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t80 Shape__int32 = Shape__int32_Wrapped{
        _0: t79,
    }
    var t81 string = shape_int32_to_string(t80)
    println__T_string(t81)
    var t82 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t82)
    var t83 Point = Point{
        x: 3,
        y: 4,
    }
    var t84 Shape__unit = Shape__unit_Dot{
        _0: t83,
    }
    var t85 string = shape_unit_to_string(t84)
    println__T_string(t85)
    var t86 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t87 Shape__unit = Shape__unit_Wrapped{
        _0: t86,
    }
    var t88 string = shape_unit_to_string(t87)
    println__T_string(t88)
    var t89 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t89)
    var t90 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t90)
    println__T_string("struct enums!")
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv95 int32
    var jp97 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp97 = 1
    case Shape__int32_Wrapped:
        jp97 = 2
    case Shape__int32_Origin:
        jp97 = 0
    default:
        panic("non-exhaustive match")
    }
    retv95 = jp97
    return retv95
}

func main() {
    main0()
}
