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
    var retv178 Shape__int32
    var jp180 Shape__int32
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x152 Point = shape__0.(Shape__int32_Dot)._0
        var point__1 Point = x152
        var t181 Shape__int32 = Shape__int32_Dot{
            _0: point__1,
        }
        jp180 = t181
    case Shape__int32_Wrapped:
        var x153 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var inner__2 Wrapper__int32 = x153
        var t182 Shape__int32 = Shape__int32_Wrapped{
            _0: inner__2,
        }
        jp180 = t182
    case Shape__int32_Origin:
        jp180 = Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
    retv178 = jp180
    return retv178
}

func point32_to_string(point__8 Point) string {
    var retv193 string
    var mtmp156 Point = point__8
    var x157 int32 = mtmp156.x
    var x158 int32 = mtmp156.y
    var y__10 int32 = x158
    var x__9 int32 = x157
    var t194 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__9)
    var with_x__11 string = "Point { x: " + t194
    var with_y_label__12 string = with_x__11 + ", y: "
    var t195 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__10)
    var with_y__13 string = with_y_label__12 + t195
    var t196 string = with_y__13 + " }"
    retv193 = t196
    return retv193
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var retv198 string
    var mtmp159 Wrapper__int32 = wrapper__14
    var x160 int32 = mtmp159.value
    var value__15 int32 = x160
    var t199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t199
    var t200 string = prefix__16 + " }"
    retv198 = t200
    return retv198
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var retv202 string
    var mtmp161 Wrapper__unit = wrapper__17
    var x162 struct{} = mtmp161.value
    var value__18 struct{} = x162
    var t203 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t203
    var t204 string = prefix__19 + " }"
    retv202 = t204
    return retv202
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var retv206 string
    var jp208 string
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x163 Point = shape__20.(Shape__int32_Dot)._0
        var point__21 Point = x163
        var t209 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t209
        var t210 string = prefix__22 + ")"
        jp208 = t210
    case Shape__int32_Wrapped:
        var x164 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var wrapper__23 Wrapper__int32 = x164
        var t211 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t211
        var t212 string = prefix__24 + ")"
        jp208 = t212
    case Shape__int32_Origin:
        jp208 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv206 = jp208
    return retv206
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var retv214 string
    var jp216 string
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x165 Point = shape__25.(Shape__unit_Dot)._0
        var point__26 Point = x165
        var t217 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t217
        var t218 string = prefix__27 + ")"
        jp216 = t218
    case Shape__unit_Wrapped:
        var x166 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var wrapper__28 Wrapper__unit = x166
        var t219 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t219
        var t220 string = prefix__29 + ")"
        jp216 = t220
    case Shape__unit_Origin:
        jp216 = "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
    retv214 = jp216
    return retv214
}

func main0() struct{} {
    var t222 Point = Point{
        x: 3,
        y: 4,
    }
    var t223 string = point32_to_string(t222)
    println__T_string(t223)
    var t224 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t225 string = wrapper_int32_to_string(t224)
    println__T_string(t225)
    var t226 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t227 string = wrapper_unit_to_string(t226)
    println__T_string(t227)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t228 Point = Point{
        x: 3,
        y: 4,
    }
    var t229 Shape__int32 = Shape__int32_Dot{
        _0: t228,
    }
    var t230 string = shape_int32_to_string(t229)
    println__T_string(t230)
    var t231 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t232 Shape__int32 = Shape__int32_Wrapped{
        _0: t231,
    }
    var t233 string = shape_int32_to_string(t232)
    println__T_string(t233)
    var t234 string = shape_int32_to_string(bounced_origin__30)
    println__T_string(t234)
    var t235 Point = Point{
        x: 3,
        y: 4,
    }
    var t236 Shape__unit = Shape__unit_Dot{
        _0: t235,
    }
    var t237 string = shape_unit_to_string(t236)
    println__T_string(t237)
    var t238 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t239 Shape__unit = Shape__unit_Wrapped{
        _0: t238,
    }
    var t240 string = shape_unit_to_string(t239)
    println__T_string(t240)
    var t241 string = shape_unit_to_string(Shape__unit_Origin{})
    println__T_string(t241)
    var t242 Shape__int32 = bounce_int(Shape__int32_Origin{})
    describe__T_int32(t242)
    println__T_string("struct enums!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv245 string
    var t246 string = _goml_runtime_core_int32_to_string(self__6)
    retv245 = t246
    return retv245
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv248 string
    var t249 string = _goml_runtime_core_unit_to_string(self__36)
    retv248 = t249
    return retv248
}

func println__T_string(value__1 string) struct{} {
    var t251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t251)
    return struct{}{}
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var retv254 int32
    var jp256 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        jp256 = 1
    case Shape__int32_Wrapped:
        jp256 = 2
    case Shape__int32_Origin:
        jp256 = 0
    default:
        panic("non-exhaustive match")
    }
    retv254 = jp256
    return retv254
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv258 string
    retv258 = self__38
    return retv258
}

func main() {
    main0()
}
