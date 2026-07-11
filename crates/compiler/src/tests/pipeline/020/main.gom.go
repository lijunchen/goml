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
    var retv33 Shape__int32
    var jp35 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x7 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x7
        var t36 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp35 = t36
    case Shape__int32_Wrapped:
        var x8 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x8
        var t37 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp35 = t37
    case Shape__int32_Origin:
        jp35 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv33 = jp35
    return retv33
}

func point32_to_string(point__8 Point) string {
    var retv48 string
    var mtmp11 Point = point__8
    var x12 int32 = mtmp11.x
    var x13 int32 = mtmp11.y
    var y__10 int32 = x13
    var x__9 int32 = x12
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t49
    var with_y_label__12 string = with_x__11 + ", y: "
    var t50 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t50
    var t51 string = with_y__13 + " }"
    retv48 = t51
    return retv48
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv53 string
    var mtmp14 Wrapper__int32 = wrapper__14
    var x15 int32 = mtmp14.value
    var value__15 int32 = x15
    var t54 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t54
    var t55 string = prefix__16 + " }"
    retv53 = t55
    return retv53
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv57 string
    var mtmp16 Wrapper__unit = wrapper__17
    var x17 struct{} = mtmp16.value
    var value__18 struct{} = x17
    var t58 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t58
    var t59 string = prefix__19 + " }"
    retv57 = t59
    return retv57
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv61 string
    var jp63 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x18 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x18
        var t64 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t64
        var t65 string = prefix__22 + ")"
        jp63 = t65
    case Shape__int32_Wrapped:
        var x19 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x19
        var t66 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t66
        var t67 string = prefix__24 + ")"
        jp63 = t67
    case Shape__int32_Origin:
        jp63 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv61 = jp63
    return retv61
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv69 string
    var jp71 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x20 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x20
        var t72 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t72
        var t73 string = prefix__27 + ")"
        jp71 = t73
    case Shape__unit_Wrapped:
        var x21 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x21
        var t74 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t74
        var t75 string = prefix__29 + ")"
        jp71 = t75
    case Shape__unit_Origin:
        jp71 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func main0() struct{} {
    var t77 Point = Point{
        x: 3,
        y: 4,
    }
    var t78 string = point32_to_string(t77)
    println__T_string(t78)
    var t79 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t80 string = wrapper_int32_to_string(t79)
    println__T_string(t80)
    var t81 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t82 string = wrapper_unit_to_string(t81)
    println__T_string(t82)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t83 Point = Point{
        x: 3,
        y: 4,
    }
    var t84 Shape__int32 = Shape__int32_Dot{
        _0: t83,
    }
    var t85 string = shape_int32_to_string(t84)
    println__T_string(t85)
    var t86 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t87 Shape__int32 = Shape__int32_Wrapped{
        _0: t86,
    }
    var t88 string = shape_int32_to_string(t87)
    println__T_string(t88)
    var t89 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t89)
    var t90 Point = Point{
        x: 3,
        y: 4,
    }
    var t91 Shape__unit = Shape__unit_Dot{
        _0: t90,
    }
    var t92 string = shape_unit_to_string(t91)
    println__T_string(t92)
    var t93 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t94 Shape__unit = Shape__unit_Wrapped{
        _0: t93,
    }
    var t95 string = shape_unit_to_string(t94)
    println__T_string(t95)
    var t96 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t96)
    var t97 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t97)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_int32_to_string(self__2)
    retv100 = t101
    return retv100
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv103 string
    var t104 string = _goml_runtime_core_unit_to_string(self__7)
    retv103 = t104
    return retv103
}

func println__T_string(value__1 string) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv109 int32
    var jp111 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp111 = 1
    case Shape__int32_Wrapped:
        jp111 = 2
    case Shape__int32_Origin:
        jp111 = 0
    default:
        panic("non-exhaustive match")
    }
    retv109 = jp111
    return retv109
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv113 string
    retv113 = self__9
    return retv113
}

func main() {
    main0()
}
