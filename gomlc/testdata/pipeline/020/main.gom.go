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
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x136 Point = shape__0.(Shape__int32_Dot)._0
        var t165 Shape__int32 = Shape__int32_Dot{
            _0: x136,
        }
        return t165
    case Shape__int32_Wrapped:
        var x137 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t166 Shape__int32 = Shape__int32_Wrapped{
            _0: x137,
        }
        return t166
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x141 int32 = point__8.x
    var x142 int32 = point__8.y
    var t178 string
    var inline246 string = _goml_runtime_core_int32_to_string(x141)
    t178 = inline246
    var with_x__11 string = "Point { x: " + t178
    var with_y_label__12 string = with_x__11 + ", y: "
    var t179 string
    var inline244 string = _goml_runtime_core_int32_to_string(x142)
    t179 = inline244
    var with_y__13 string = with_y_label__12 + t179
    var t180 string = with_y__13 + " }"
    return t180
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x144 int32 = wrapper__14.value
    var t183 string
    var inline248 string = _goml_runtime_core_int32_to_string(x144)
    t183 = inline248
    var prefix__16 string = "Wrapper[int32] { value: " + t183
    var t184 string = prefix__16 + " }"
    return t184
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x146 struct{} = wrapper__17.value
    var t187 string
    var inline250 string = _goml_runtime_core_unit_to_string(x146)
    t187 = inline250
    var prefix__19 string = "Wrapper[unit] { value: " + t187
    var t188 string = prefix__19 + " }"
    return t188
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x147 Point = shape__20.(Shape__int32_Dot)._0
        var t193 string
        var inline253 int32 = x147.x
        var inline254 int32 = x147.y
        var inline257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
        var inline258 string = "Point { x: " + inline257
        var inline259 string = inline258 + ", y: "
        var inline260 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline254)
        var inline261 string = inline259 + inline260
        var inline262 string = inline261 + " }"
        t193 = inline262
        var prefix__22 string = "Shape::Dot(" + t193
        var t194 string = prefix__22 + ")"
        return t194
    case Shape__int32_Wrapped:
        var x148 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t195 string
        var inline265 int32 = x148.value
        var inline267 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline265)
        var inline268 string = "Wrapper[int32] { value: " + inline267
        var inline269 string = inline268 + " }"
        t195 = inline269
        var prefix__24 string = "Shape::Wrapped(" + t195
        var t196 string = prefix__24 + ")"
        return t196
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x149 Point = shape__25.(Shape__unit_Dot)._0
        var t201 string
        var inline272 int32 = x149.x
        var inline273 int32 = x149.y
        var inline276 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline272)
        var inline277 string = "Point { x: " + inline276
        var inline278 string = inline277 + ", y: "
        var inline279 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline273)
        var inline280 string = inline278 + inline279
        var inline281 string = inline280 + " }"
        t201 = inline281
        var prefix__27 string = "Shape::Dot(" + t201
        var t202 string = prefix__27 + ")"
        return t202
    case Shape__unit_Wrapped:
        var x150 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t203 string
        var inline284 struct{} = x150.value
        var inline286 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline284)
        var inline287 string = "Wrapper[unit] { value: " + inline286
        var inline288 string = inline287 + " }"
        t203 = inline288
        var prefix__29 string = "Shape::Wrapped(" + t203
        var t204 string = prefix__29 + ")"
        return t204
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t206 Point = Point{
        x: 3,
        y: 4,
    }
    var t207 string = point32_to_string(t206)
    println__T_string(t207)
    var t208 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t209 string = wrapper_int32_to_string(t208)
    println__T_string(t209)
    var t210 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t211 string = wrapper_unit_to_string(t210)
    println__T_string(t211)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t212 Point = Point{
        x: 3,
        y: 4,
    }
    var t213 Shape__int32 = Shape__int32_Dot{
        _0: t212,
    }
    var t214 string = shape_int32_to_string(t213)
    println__T_string(t214)
    var t215 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t216 Shape__int32 = Shape__int32_Wrapped{
        _0: t215,
    }
    var t217 string = shape_int32_to_string(t216)
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline327)
    var t218 string = shape_int32_to_string(bounced_origin__30)
    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline324)
    var t219 Point = Point{
        x: 3,
        y: 4,
    }
    var t220 Shape__unit = Shape__unit_Dot{
        _0: t219,
    }
    var t221 string = shape_unit_to_string(t220)
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline321)
    var t222 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t223 Shape__unit = Shape__unit_Wrapped{
        _0: t222,
    }
    var t224 string = shape_unit_to_string(t223)
    var inline318 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline318)
    var t225 string
    t225 = "Shape::Origin"
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline304)
    var t226 Shape__int32
    t226 = Shape__int32_Origin{}
    switch t226.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline290 string = "struct enums!"
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline290)
    _goml_runtime_core_string_println(inline291)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t230 string = _goml_runtime_core_int32_to_string(self__35)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t233 string = _goml_runtime_core_unit_to_string(self__65)
    return t233
}

func println__T_string(value__31 string) struct{} {
    var t235 string
    t235 = value__31
    _goml_runtime_core_string_println(t235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
