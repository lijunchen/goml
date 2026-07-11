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
    var retv48 Shape__int32
    var jp50 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x22 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x22
        var t51 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp50 = t51
    case Shape__int32_Wrapped:
        var x23 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x23
        var t52 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp50 = t52
    case Shape__int32_Origin:
        jp50 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv48 = jp50
    return retv48
}

func point32_to_string(point__8 Point) string {
    var retv63 string
    var mtmp26 Point = point__8
    var x27 int32 = mtmp26.x
    var x28 int32 = mtmp26.y
    var y__10 int32 = x28
    var x__9 int32 = x27
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t64
    var with_y_label__12 string = with_x__11 + ", y: "
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t65
    var t66 string = with_y__13 + " }"
    retv63 = t66
    return retv63
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv68 string
    var mtmp29 Wrapper__int32 = wrapper__14
    var x30 int32 = mtmp29.value
    var value__15 int32 = x30
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t69
    var t70 string = prefix__16 + " }"
    retv68 = t70
    return retv68
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv72 string
    var mtmp31 Wrapper__unit = wrapper__17
    var x32 struct{} = mtmp31.value
    var value__18 struct{} = x32
    var t73 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t73
    var t74 string = prefix__19 + " }"
    retv72 = t74
    return retv72
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv76 string
    var jp78 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x33 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x33
        var t79 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t79
        var t80 string = prefix__22 + ")"
        jp78 = t80
    case Shape__int32_Wrapped:
        var x34 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x34
        var t81 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t81
        var t82 string = prefix__24 + ")"
        jp78 = t82
    case Shape__int32_Origin:
        jp78 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv84 string
    var jp86 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x35 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x35
        var t87 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t87
        var t88 string = prefix__27 + ")"
        jp86 = t88
    case Shape__unit_Wrapped:
        var x36 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x36
        var t89 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t89
        var t90 string = prefix__29 + ")"
        jp86 = t90
    case Shape__unit_Origin:
        jp86 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t92 Point = Point{
        x: 3,
        y: 4,
    }
    var t93 string = point32_to_string(t92)
    println__T_string(t93)
    var t94 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t95 string = wrapper_int32_to_string(t94)
    println__T_string(t95)
    var t96 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t97 string = wrapper_unit_to_string(t96)
    println__T_string(t97)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t98 Point = Point{
        x: 3,
        y: 4,
    }
    var t99 Shape__int32 = Shape__int32_Dot{
        _0: t98,
    }
    var t100 string = shape_int32_to_string(t99)
    println__T_string(t100)
    var t101 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t102 Shape__int32 = Shape__int32_Wrapped{
        _0: t101,
    }
    var t103 string = shape_int32_to_string(t102)
    println__T_string(t103)
    var t104 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t104)
    var t105 Point = Point{
        x: 3,
        y: 4,
    }
    var t106 Shape__unit = Shape__unit_Dot{
        _0: t105,
    }
    var t107 string = shape_unit_to_string(t106)
    println__T_string(t107)
    var t108 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t109 Shape__unit = Shape__unit_Wrapped{
        _0: t108,
    }
    var t110 string = shape_unit_to_string(t109)
    println__T_string(t110)
    var t111 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t111)
    var t112 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t112)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv115 string
    var t116 string = _goml_runtime_core_int32_to_string(self__2)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv118 string
    var t119 string = _goml_runtime_core_unit_to_string(self__7)
    retv118 = t119
    return retv118
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv124 int32
    var jp126 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp126 = 1
    case Shape__int32_Wrapped:
        jp126 = 2
    case Shape__int32_Origin:
        jp126 = 0
    default:
        panic("non-exhaustive match")
    }
    retv124 = jp126
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv128 string
    retv128 = self__9
    return retv128
}

func main() {
    main0()
}
