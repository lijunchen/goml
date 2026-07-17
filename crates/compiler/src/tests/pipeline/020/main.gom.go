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
    var retv84 Shape__int32
    var jp86 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x58 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x58
        var t87 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp86 = t87
    case Shape__int32_Wrapped:
        var x59 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x59
        var t88 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp86 = t88
    case Shape__int32_Origin:
        jp86 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func point32_to_string(point__8 Point) string {
    var retv99 string
    var mtmp62 Point = point__8
    var x63 int32 = mtmp62.x
    var x64 int32 = mtmp62.y
    var y__10 int32 = x64
    var x__9 int32 = x63
    var t100 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t100
    var with_y_label__12 string = with_x__11 + ", y: "
    var t101 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t101
    var t102 string = with_y__13 + " }"
    retv99 = t102
    return retv99
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv104 string
    var mtmp65 Wrapper__int32 = wrapper__14
    var x66 int32 = mtmp65.value
    var value__15 int32 = x66
    var t105 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t105
    var t106 string = prefix__16 + " }"
    retv104 = t106
    return retv104
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv108 string
    var mtmp67 Wrapper__unit = wrapper__17
    var x68 struct{} = mtmp67.value
    var value__18 struct{} = x68
    var t109 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t109
    var t110 string = prefix__19 + " }"
    retv108 = t110
    return retv108
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv112 string
    var jp114 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x69 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x69
        var t115 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t115
        var t116 string = prefix__22 + ")"
        jp114 = t116
    case Shape__int32_Wrapped:
        var x70 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x70
        var t117 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t117
        var t118 string = prefix__24 + ")"
        jp114 = t118
    case Shape__int32_Origin:
        jp114 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv112 = jp114
    return retv112
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv120 string
    var jp122 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x71 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x71
        var t123 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t123
        var t124 string = prefix__27 + ")"
        jp122 = t124
    case Shape__unit_Wrapped:
        var x72 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x72
        var t125 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t125
        var t126 string = prefix__29 + ")"
        jp122 = t126
    case Shape__unit_Origin:
        jp122 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv120 = jp122
    return retv120
}

func main0() struct{} {
    var t128 Point = Point{
        x: 3,
        y: 4,
    }
    var t129 string = point32_to_string(t128)
    println__T_string(t129)
    var t130 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t131 string = wrapper_int32_to_string(t130)
    println__T_string(t131)
    var t132 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t133 string = wrapper_unit_to_string(t132)
    println__T_string(t133)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t134 Point = Point{
        x: 3,
        y: 4,
    }
    var t135 Shape__int32 = Shape__int32_Dot{
        _0: t134,
    }
    var t136 string = shape_int32_to_string(t135)
    println__T_string(t136)
    var t137 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t138 Shape__int32 = Shape__int32_Wrapped{
        _0: t137,
    }
    var t139 string = shape_int32_to_string(t138)
    println__T_string(t139)
    var t140 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t140)
    var t141 Point = Point{
        x: 3,
        y: 4,
    }
    var t142 Shape__unit = Shape__unit_Dot{
        _0: t141,
    }
    var t143 string = shape_unit_to_string(t142)
    println__T_string(t143)
    var t144 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t145 Shape__unit = Shape__unit_Wrapped{
        _0: t144,
    }
    var t146 string = shape_unit_to_string(t145)
    println__T_string(t146)
    var t147 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t147)
    var t148 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t148)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int32_to_string(self__2)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__32 struct{}) string {
    var retv154 string
    var t155 string = _goml_runtime_core_unit_to_string(self__32)
    retv154 = t155
    return retv154
}

func println__T_string(value__1 string) struct{} {
    var t157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv160 int32
    var jp162 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp162 = 1
    case Shape__int32_Wrapped:
        jp162 = 2
    case Shape__int32_Origin:
        jp162 = 0
    default:
        panic("non-exhaustive match")
    }
    retv160 = jp162
    return retv160
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv164 string
    retv164 = self__34
    return retv164
}

func main() {
    main0()
}
