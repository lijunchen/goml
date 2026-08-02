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
        var x155 Point = shape__0.(Shape__int32_Dot)._0
        var t184 Shape__int32 = Shape__int32_Dot{
            _0: x155,
        }
        return t184
    case Shape__int32_Wrapped:
        var x156 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t185 Shape__int32 = Shape__int32_Wrapped{
            _0: x156,
        }
        return t185
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x160 int32 = point__8.x
    var x161 int32 = point__8.y
    var t197 string
    var inline265 string = _goml_runtime_core_int32_to_string(x160)
    t197 = inline265
    var with_x__11 string = "Point { x: " + t197
    var with_y_label__12 string = with_x__11 + ", y: "
    var t198 string
    var inline263 string = _goml_runtime_core_int32_to_string(x161)
    t198 = inline263
    var with_y__13 string = with_y_label__12 + t198
    var t199 string = with_y__13 + " }"
    return t199
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x163 int32 = wrapper__14.value
    var t202 string
    var inline267 string = _goml_runtime_core_int32_to_string(x163)
    t202 = inline267
    var prefix__16 string = "Wrapper[int32] { value: " + t202
    var t203 string = prefix__16 + " }"
    return t203
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x165 struct{} = wrapper__17.value
    var t206 string
    var inline269 string = _goml_runtime_core_unit_to_string(x165)
    t206 = inline269
    var prefix__19 string = "Wrapper[unit] { value: " + t206
    var t207 string = prefix__19 + " }"
    return t207
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x166 Point = shape__20.(Shape__int32_Dot)._0
        var t212 string
        var inline272 int32 = x166.x
        var inline273 int32 = x166.y
        var inline276 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline272)
        var inline277 string = "Point { x: " + inline276
        var inline278 string = inline277 + ", y: "
        var inline279 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline273)
        var inline280 string = inline278 + inline279
        var inline281 string = inline280 + " }"
        t212 = inline281
        var prefix__22 string = "Shape::Dot(" + t212
        var t213 string = prefix__22 + ")"
        return t213
    case Shape__int32_Wrapped:
        var x167 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t214 string
        var inline284 int32 = x167.value
        var inline286 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline284)
        var inline287 string = "Wrapper[int32] { value: " + inline286
        var inline288 string = inline287 + " }"
        t214 = inline288
        var prefix__24 string = "Shape::Wrapped(" + t214
        var t215 string = prefix__24 + ")"
        return t215
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x168 Point = shape__25.(Shape__unit_Dot)._0
        var t220 string
        var inline291 int32 = x168.x
        var inline292 int32 = x168.y
        var inline295 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline291)
        var inline296 string = "Point { x: " + inline295
        var inline297 string = inline296 + ", y: "
        var inline298 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline292)
        var inline299 string = inline297 + inline298
        var inline300 string = inline299 + " }"
        t220 = inline300
        var prefix__27 string = "Shape::Dot(" + t220
        var t221 string = prefix__27 + ")"
        return t221
    case Shape__unit_Wrapped:
        var x169 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t222 string
        var inline303 struct{} = x169.value
        var inline305 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline303)
        var inline306 string = "Wrapper[unit] { value: " + inline305
        var inline307 string = inline306 + " }"
        t222 = inline307
        var prefix__29 string = "Shape::Wrapped(" + t222
        var t223 string = prefix__29 + ")"
        return t223
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t225 Point = Point{
        x: 3,
        y: 4,
    }
    var t226 string = point32_to_string(t225)
    println__T_string(t226)
    var t227 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t228 string = wrapper_int32_to_string(t227)
    println__T_string(t228)
    var t229 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t230 string = wrapper_unit_to_string(t229)
    println__T_string(t230)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t231 Point = Point{
        x: 3,
        y: 4,
    }
    var t232 Shape__int32 = Shape__int32_Dot{
        _0: t231,
    }
    var t233 string = shape_int32_to_string(t232)
    println__T_string(t233)
    var t234 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t235 Shape__int32 = Shape__int32_Wrapped{
        _0: t234,
    }
    var t236 string = shape_int32_to_string(t235)
    var inline346 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline346)
    var t237 string = shape_int32_to_string(bounced_origin__30)
    var inline343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline343)
    var t238 Point = Point{
        x: 3,
        y: 4,
    }
    var t239 Shape__unit = Shape__unit_Dot{
        _0: t238,
    }
    var t240 string = shape_unit_to_string(t239)
    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline340)
    var t241 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t242 Shape__unit = Shape__unit_Wrapped{
        _0: t241,
    }
    var t243 string = shape_unit_to_string(t242)
    var inline337 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline337)
    var t244 string
    t244 = "Shape::Origin"
    var inline323 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline323)
    var t245 Shape__int32
    t245 = Shape__int32_Origin{}
    switch t245.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline309 string = "struct enums!"
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline309)
    _goml_runtime_core_string_println(inline310)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t249 string = _goml_runtime_core_int32_to_string(self__6)
    return t249
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var t252 string = _goml_runtime_core_unit_to_string(self__36)
    return t252
}

func println__T_string(value__1 string) struct{} {
    var t254 string
    t254 = value__1
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
