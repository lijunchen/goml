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
    var retv90 Shape__int32
    var jp92 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x64 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x64
        var t93 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp92 = t93
    case Shape__int32_Wrapped:
        var x65 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x65
        var t94 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp92 = t94
    case Shape__int32_Origin:
        jp92 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func point32_to_string(point__8 Point) string {
    var retv105 string
    var mtmp68 Point = point__8
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__10 int32 = x70
    var x__9 int32 = x69
    var t106 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t106
    var with_y_label__12 string = with_x__11 + ", y: "
    var t107 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t107
    var t108 string = with_y__13 + " }"
    retv105 = t108
    return retv105
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv110 string
    var mtmp71 Wrapper__int32 = wrapper__14
    var x72 int32 = mtmp71.value
    var value__15 int32 = x72
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t111
    var t112 string = prefix__16 + " }"
    retv110 = t112
    return retv110
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv114 string
    var mtmp73 Wrapper__unit = wrapper__17
    var x74 struct{} = mtmp73.value
    var value__18 struct{} = x74
    var t115 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t115
    var t116 string = prefix__19 + " }"
    retv114 = t116
    return retv114
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv118 string
    var jp120 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x75 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x75
        var t121 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t121
        var t122 string = prefix__22 + ")"
        jp120 = t122
    case Shape__int32_Wrapped:
        var x76 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x76
        var t123 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t123
        var t124 string = prefix__24 + ")"
        jp120 = t124
    case Shape__int32_Origin:
        jp120 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv118 = jp120
    return retv118
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv126 string
    var jp128 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x77 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x77
        var t129 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t129
        var t130 string = prefix__27 + ")"
        jp128 = t130
    case Shape__unit_Wrapped:
        var x78 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x78
        var t131 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t131
        var t132 string = prefix__29 + ")"
        jp128 = t132
    case Shape__unit_Origin:
        jp128 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv126 = jp128
    return retv126
}

func main0() struct{} {
    var t134 Point = Point{
        x: 3,
        y: 4,
    }
    var t135 string = point32_to_string(t134)
    println__T_string(t135)
    var t136 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t137 string = wrapper_int32_to_string(t136)
    println__T_string(t137)
    var t138 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t139 string = wrapper_unit_to_string(t138)
    println__T_string(t139)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t140 Point = Point{
        x: 3,
        y: 4,
    }
    var t141 Shape__int32 = Shape__int32_Dot{
        _0: t140,
    }
    var t142 string = shape_int32_to_string(t141)
    println__T_string(t142)
    var t143 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t144 Shape__int32 = Shape__int32_Wrapped{
        _0: t143,
    }
    var t145 string = shape_int32_to_string(t144)
    println__T_string(t145)
    var t146 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t146)
    var t147 Point = Point{
        x: 3,
        y: 4,
    }
    var t148 Shape__unit = Shape__unit_Dot{
        _0: t147,
    }
    var t149 string = shape_unit_to_string(t148)
    println__T_string(t149)
    var t150 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t151 Shape__unit = Shape__unit_Wrapped{
        _0: t150,
    }
    var t152 string = shape_unit_to_string(t151)
    println__T_string(t152)
    var t153 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t153)
    var t154 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t154)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv157 string
    var t158 string = _goml_runtime_core_int32_to_string(self__6)
    retv157 = t158
    return retv157
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv160 string
    var t161 string = _goml_runtime_core_unit_to_string(self__36)
    retv160 = t161
    return retv160
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv166 int32
    var jp168 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp168 = 1
    case Shape__int32_Wrapped:
        jp168 = 2
    case Shape__int32_Origin:
        jp168 = 0
    default:
        panic("non-exhaustive match")
    }
    retv166 = jp168
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv170 string
    retv170 = self__38
    return retv170
}

func main() {
    main0()
}
