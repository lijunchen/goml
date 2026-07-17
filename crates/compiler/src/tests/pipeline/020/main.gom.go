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
    var retv87 Shape__int32
    var jp89 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x61 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x61
        var t90 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp89 = t90
    case Shape__int32_Wrapped:
        var x62 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x62
        var t91 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp89 = t91
    case Shape__int32_Origin:
        jp89 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func point32_to_string(point__8 Point) string {
    var retv102 string
    var mtmp65 Point = point__8
    var x66 int32 = mtmp65.x
    var x67 int32 = mtmp65.y
    var y__10 int32 = x67
    var x__9 int32 = x66
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t103
    var with_y_label__12 string = with_x__11 + ", y: "
    var t104 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t104
    var t105 string = with_y__13 + " }"
    retv102 = t105
    return retv102
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv107 string
    var mtmp68 Wrapper__int32 = wrapper__14
    var x69 int32 = mtmp68.value
    var value__15 int32 = x69
    var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t108
    var t109 string = prefix__16 + " }"
    retv107 = t109
    return retv107
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv111 string
    var mtmp70 Wrapper__unit = wrapper__17
    var x71 struct{} = mtmp70.value
    var value__18 struct{} = x71
    var t112 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t112
    var t113 string = prefix__19 + " }"
    retv111 = t113
    return retv111
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv115 string
    var jp117 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x72 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x72
        var t118 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t118
        var t119 string = prefix__22 + ")"
        jp117 = t119
    case Shape__int32_Wrapped:
        var x73 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x73
        var t120 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t120
        var t121 string = prefix__24 + ")"
        jp117 = t121
    case Shape__int32_Origin:
        jp117 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv115 = jp117
    return retv115
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv123 string
    var jp125 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x74 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x74
        var t126 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t126
        var t127 string = prefix__27 + ")"
        jp125 = t127
    case Shape__unit_Wrapped:
        var x75 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x75
        var t128 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t128
        var t129 string = prefix__29 + ")"
        jp125 = t129
    case Shape__unit_Origin:
        jp125 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func main0() struct{} {
    var t131 Point = Point{
        x: 3,
        y: 4,
    }
    var t132 string = point32_to_string(t131)
    println__T_string(t132)
    var t133 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t134 string = wrapper_int32_to_string(t133)
    println__T_string(t134)
    var t135 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t136 string = wrapper_unit_to_string(t135)
    println__T_string(t136)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t137 Point = Point{
        x: 3,
        y: 4,
    }
    var t138 Shape__int32 = Shape__int32_Dot{
        _0: t137,
    }
    var t139 string = shape_int32_to_string(t138)
    println__T_string(t139)
    var t140 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t141 Shape__int32 = Shape__int32_Wrapped{
        _0: t140,
    }
    var t142 string = shape_int32_to_string(t141)
    println__T_string(t142)
    var t143 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t143)
    var t144 Point = Point{
        x: 3,
        y: 4,
    }
    var t145 Shape__unit = Shape__unit_Dot{
        _0: t144,
    }
    var t146 string = shape_unit_to_string(t145)
    println__T_string(t146)
    var t147 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t148 Shape__unit = Shape__unit_Wrapped{
        _0: t147,
    }
    var t149 string = shape_unit_to_string(t148)
    println__T_string(t149)
    var t150 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t150)
    var t151 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t151)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv154 string
    var t155 string = _goml_runtime_core_int32_to_string(self__5)
    retv154 = t155
    return retv154
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__35 struct{}) string {
    var retv157 string
    var t158 string = _goml_runtime_core_unit_to_string(self__35)
    retv157 = t158
    return retv157
}

func println__T_string(value__1 string) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv163 int32
    var jp165 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp165 = 1
    case Shape__int32_Wrapped:
        jp165 = 2
    case Shape__int32_Origin:
        jp165 = 0
    default:
        panic("non-exhaustive match")
    }
    retv163 = jp165
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv167 string
    retv167 = self__37
    return retv167
}

func main() {
    main0()
}
