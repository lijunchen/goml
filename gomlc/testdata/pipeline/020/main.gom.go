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
    var retv94 Shape__int32
    var jp96 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x68 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x68
        var t97 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp96 = t97
    case Shape__int32_Wrapped:
        var x69 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x69
        var t98 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp96 = t98
    case Shape__int32_Origin:
        jp96 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func point32_to_string(point__8 Point) string {
    var retv109 string
    var mtmp72 Point = point__8
    var x73 int32 = mtmp72.x
    var x74 int32 = mtmp72.y
    var y__10 int32 = x74
    var x__9 int32 = x73
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t110
    var with_y_label__12 string = with_x__11 + ", y: "
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t111
    var t112 string = with_y__13 + " }"
    retv109 = t112
    return retv109
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv114 string
    var mtmp75 Wrapper__int32 = wrapper__14
    var x76 int32 = mtmp75.value
    var value__15 int32 = x76
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t115
    var t116 string = prefix__16 + " }"
    retv114 = t116
    return retv114
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv118 string
    var mtmp77 Wrapper__unit = wrapper__17
    var x78 struct{} = mtmp77.value
    var value__18 struct{} = x78
    var t119 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t119
    var t120 string = prefix__19 + " }"
    retv118 = t120
    return retv118
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv122 string
    var jp124 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x79 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x79
        var t125 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t125
        var t126 string = prefix__22 + ")"
        jp124 = t126
    case Shape__int32_Wrapped:
        var x80 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x80
        var t127 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t127
        var t128 string = prefix__24 + ")"
        jp124 = t128
    case Shape__int32_Origin:
        jp124 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv122 = jp124
    return retv122
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv130 string
    var jp132 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x81 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x81
        var t133 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t133
        var t134 string = prefix__27 + ")"
        jp132 = t134
    case Shape__unit_Wrapped:
        var x82 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x82
        var t135 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t135
        var t136 string = prefix__29 + ")"
        jp132 = t136
    case Shape__unit_Origin:
        jp132 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv130 = jp132
    return retv130
}

func main0() struct{} {
    var t138 Point = Point{
        x: 3,
        y: 4,
    }
    var t139 string = point32_to_string(t138)
    println__T_string(t139)
    var t140 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t141 string = wrapper_int32_to_string(t140)
    println__T_string(t141)
    var t142 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t143 string = wrapper_unit_to_string(t142)
    println__T_string(t143)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t144 Point = Point{
        x: 3,
        y: 4,
    }
    var t145 Shape__int32 = Shape__int32_Dot{
        _0: t144,
    }
    var t146 string = shape_int32_to_string(t145)
    println__T_string(t146)
    var t147 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t148 Shape__int32 = Shape__int32_Wrapped{
        _0: t147,
    }
    var t149 string = shape_int32_to_string(t148)
    println__T_string(t149)
    var t150 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t150)
    var t151 Point = Point{
        x: 3,
        y: 4,
    }
    var t152 Shape__unit = Shape__unit_Dot{
        _0: t151,
    }
    var t153 string = shape_unit_to_string(t152)
    println__T_string(t153)
    var t154 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t155 Shape__unit = Shape__unit_Wrapped{
        _0: t154,
    }
    var t156 string = shape_unit_to_string(t155)
    println__T_string(t156)
    var t157 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t157)
    var t158 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t158)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv161 string
    var t162 string = _goml_runtime_core_int32_to_string(self__6)
    retv161 = t162
    return retv161
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv164 string
    var t165 string = _goml_runtime_core_unit_to_string(self__36)
    retv164 = t165
    return retv164
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv170 int32
    var jp172 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp172 = 1
    case Shape__int32_Wrapped:
        jp172 = 2
    case Shape__int32_Origin:
        jp172 = 0
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv174 string
    retv174 = self__38
    return retv174
}

func main() {
    main0()
}
