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
    var retv30 Shape__int32
    var jp32 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x4 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x4
        var t33 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp32 = t33
    case Shape__int32_Wrapped:
        var x5 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x5
        var t34 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp32 = t34
    case Shape__int32_Origin:
        jp32 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func point32_to_string(point__8 Point) string {
    var retv45 string
    var mtmp8 Point = point__8
    var x9 int32 = mtmp8.x
    var x10 int32 = mtmp8.y
    var y__10 int32 = x10
    var x__9 int32 = x9
    var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t46
    var with_y_label__12 string = with_x__11 + ", y: "
    var t47 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t47
    var t48 string = with_y__13 + " }"
    retv45 = t48
    return retv45
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv50 string
    var mtmp11 Wrapper__int32 = wrapper__14
    var x12 int32 = mtmp11.value
    var value__15 int32 = x12
    var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t51
    var t52 string = prefix__16 + " }"
    retv50 = t52
    return retv50
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv54 string
    var mtmp13 Wrapper__unit = wrapper__17
    var x14 struct{} = mtmp13.value
    var value__18 struct{} = x14
    var t55 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t55
    var t56 string = prefix__19 + " }"
    retv54 = t56
    return retv54
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv58 string
    var jp60 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x15 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x15
        var t61 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t61
        var t62 string = prefix__22 + ")"
        jp60 = t62
    case Shape__int32_Wrapped:
        var x16 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x16
        var t63 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t63
        var t64 string = prefix__24 + ")"
        jp60 = t64
    case Shape__int32_Origin:
        jp60 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv58 = jp60
    return retv58
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv66 string
    var jp68 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x17 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x17
        var t69 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t69
        var t70 string = prefix__27 + ")"
        jp68 = t70
    case Shape__unit_Wrapped:
        var x18 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x18
        var t71 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t71
        var t72 string = prefix__29 + ")"
        jp68 = t72
    case Shape__unit_Origin:
        jp68 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func main0() struct{} {
    var t74 Point = Point{
        x: 3,
        y: 4,
    }
    var t75 string = point32_to_string(t74)
    println__T_string(t75)
    var t76 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t77 string = wrapper_int32_to_string(t76)
    println__T_string(t77)
    var t78 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t79 string = wrapper_unit_to_string(t78)
    println__T_string(t79)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t80 Point = Point{
        x: 3,
        y: 4,
    }
    var t81 Shape__int32 = Shape__int32_Dot{
        _0: t80,
    }
    var t82 string = shape_int32_to_string(t81)
    println__T_string(t82)
    var t83 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t84 Shape__int32 = Shape__int32_Wrapped{
        _0: t83,
    }
    var t85 string = shape_int32_to_string(t84)
    println__T_string(t85)
    var t86 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t86)
    var t87 Point = Point{
        x: 3,
        y: 4,
    }
    var t88 Shape__unit = Shape__unit_Dot{
        _0: t87,
    }
    var t89 string = shape_unit_to_string(t88)
    println__T_string(t89)
    var t90 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t91 Shape__unit = Shape__unit_Wrapped{
        _0: t90,
    }
    var t92 string = shape_unit_to_string(t91)
    println__T_string(t92)
    var t93 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t93)
    var t94 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t94)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__2)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv100 string
    var t101 string = _goml_runtime_core_unit_to_string(self__7)
    retv100 = t101
    return retv100
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv106 int32
    var jp108 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp108 = 1
    case Shape__int32_Wrapped:
        jp108 = 2
    case Shape__int32_Origin:
        jp108 = 0
    default:
        panic("non-exhaustive match")
    }
    retv106 = jp108
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv110 string
    retv110 = self__9
    return retv110
}

func main() {
    main0()
}
