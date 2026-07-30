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
    var retv134 Shape__int32
    var jp136 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x108 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x108
        var t137 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp136 = t137
    case Shape__int32_Wrapped:
        var x109 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x109
        var t138 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp136 = t138
    case Shape__int32_Origin:
        jp136 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv134 = jp136
    return retv134
}

func point32_to_string(point__8 Point) string {
    var retv149 string
    var mtmp112 Point = point__8
    var x113 int32 = mtmp112.x
    var x114 int32 = mtmp112.y
    var y__10 int32 = x114
    var x__9 int32 = x113
    var t150 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t150
    var with_y_label__12 string = with_x__11 + ", y: "
    var t151 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t151
    var t152 string = with_y__13 + " }"
    retv149 = t152
    return retv149
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv154 string
    var mtmp115 Wrapper__int32 = wrapper__14
    var x116 int32 = mtmp115.value
    var value__15 int32 = x116
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t155
    var t156 string = prefix__16 + " }"
    retv154 = t156
    return retv154
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv158 string
    var mtmp117 Wrapper__unit = wrapper__17
    var x118 struct{} = mtmp117.value
    var value__18 struct{} = x118
    var t159 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t159
    var t160 string = prefix__19 + " }"
    retv158 = t160
    return retv158
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv162 string
    var jp164 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x119 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x119
        var t165 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t165
        var t166 string = prefix__22 + ")"
        jp164 = t166
    case Shape__int32_Wrapped:
        var x120 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x120
        var t167 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t167
        var t168 string = prefix__24 + ")"
        jp164 = t168
    case Shape__int32_Origin:
        jp164 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv162 = jp164
    return retv162
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv170 string
    var jp172 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x121 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x121
        var t173 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t173
        var t174 string = prefix__27 + ")"
        jp172 = t174
    case Shape__unit_Wrapped:
        var x122 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x122
        var t175 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t175
        var t176 string = prefix__29 + ")"
        jp172 = t176
    case Shape__unit_Origin:
        jp172 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t178 Point = Point{
        x: 3,
        y: 4,
    }
    var t179 string = point32_to_string(t178)
    println__T_string(t179)
    var t180 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t181 string = wrapper_int32_to_string(t180)
    println__T_string(t181)
    var t182 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t183 string = wrapper_unit_to_string(t182)
    println__T_string(t183)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t184 Point = Point{
        x: 3,
        y: 4,
    }
    var t185 Shape__int32 = Shape__int32_Dot{
        _0: t184,
    }
    var t186 string = shape_int32_to_string(t185)
    println__T_string(t186)
    var t187 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t188 Shape__int32 = Shape__int32_Wrapped{
        _0: t187,
    }
    var t189 string = shape_int32_to_string(t188)
    println__T_string(t189)
    var t190 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t190)
    var t191 Point = Point{
        x: 3,
        y: 4,
    }
    var t192 Shape__unit = Shape__unit_Dot{
        _0: t191,
    }
    var t193 string = shape_unit_to_string(t192)
    println__T_string(t193)
    var t194 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t195 Shape__unit = Shape__unit_Wrapped{
        _0: t194,
    }
    var t196 string = shape_unit_to_string(t195)
    println__T_string(t196)
    var t197 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t197)
    var t198 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t198)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv201 string
    var t202 string = _goml_runtime_core_int32_to_string(self__6)
    retv201 = t202
    return retv201
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv204 string
    var t205 string = _goml_runtime_core_unit_to_string(self__36)
    retv204 = t205
    return retv204
}

func println__T_string(value__1 string) struct{} {
    var t207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv210 int32
    var jp212 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp212 = 1
    case Shape__int32_Wrapped:
        jp212 = 2
    case Shape__int32_Origin:
        jp212 = 0
    default:
        panic("non-exhaustive match")
    }
    retv210 = jp212
    return retv210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv214 string
    retv214 = self__38
    return retv214
}

func main() {
    main0()
}
