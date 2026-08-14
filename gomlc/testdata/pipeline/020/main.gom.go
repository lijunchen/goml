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
        var x182 Point = shape__0.(Shape__int32_Dot)._0
        var t211 Shape__int32 = Shape__int32_Dot{
            _0: x182,
        }
        return t211
    case Shape__int32_Wrapped:
        var x183 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t212 Shape__int32 = Shape__int32_Wrapped{
            _0: x183,
        }
        return t212
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x187 int32 = point__8.x
    var x188 int32 = point__8.y
    var t224 string
    var inline292 string = _goml_runtime_core_int32_to_string(x187)
    t224 = inline292
    var with_x__11 string = "Point { x: " + t224
    var with_y_label__12 string = with_x__11 + ", y: "
    var t225 string
    var inline290 string = _goml_runtime_core_int32_to_string(x188)
    t225 = inline290
    var with_y__13 string = with_y_label__12 + t225
    var t226 string = with_y__13 + " }"
    return t226
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x190 int32 = wrapper__14.value
    var t229 string
    var inline294 string = _goml_runtime_core_int32_to_string(x190)
    t229 = inline294
    var prefix__16 string = "Wrapper[int32] { value: " + t229
    var t230 string = prefix__16 + " }"
    return t230
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x192 struct{} = wrapper__17.value
    var t233 string
    var inline296 string = _goml_runtime_core_unit_to_string(x192)
    t233 = inline296
    var prefix__19 string = "Wrapper[unit] { value: " + t233
    var t234 string = prefix__19 + " }"
    return t234
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x193 Point = shape__20.(Shape__int32_Dot)._0
        var t239 string
        var inline299 int32 = x193.x
        var inline300 int32 = x193.y
        var inline303 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline299)
        var inline304 string = "Point { x: " + inline303
        var inline305 string = inline304 + ", y: "
        var inline306 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline300)
        var inline307 string = inline305 + inline306
        var inline308 string = inline307 + " }"
        t239 = inline308
        var prefix__22 string = "Shape::Dot(" + t239
        var t240 string = prefix__22 + ")"
        return t240
    case Shape__int32_Wrapped:
        var x194 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t241 string
        var inline311 int32 = x194.value
        var inline313 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline311)
        var inline314 string = "Wrapper[int32] { value: " + inline313
        var inline315 string = inline314 + " }"
        t241 = inline315
        var prefix__24 string = "Shape::Wrapped(" + t241
        var t242 string = prefix__24 + ")"
        return t242
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x195 Point = shape__25.(Shape__unit_Dot)._0
        var t247 string
        var inline318 int32 = x195.x
        var inline319 int32 = x195.y
        var inline322 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline318)
        var inline323 string = "Point { x: " + inline322
        var inline324 string = inline323 + ", y: "
        var inline325 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline319)
        var inline326 string = inline324 + inline325
        var inline327 string = inline326 + " }"
        t247 = inline327
        var prefix__27 string = "Shape::Dot(" + t247
        var t248 string = prefix__27 + ")"
        return t248
    case Shape__unit_Wrapped:
        var x196 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t249 string
        var inline330 struct{} = x196.value
        var inline332 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline330)
        var inline333 string = "Wrapper[unit] { value: " + inline332
        var inline334 string = inline333 + " }"
        t249 = inline334
        var prefix__29 string = "Shape::Wrapped(" + t249
        var t250 string = prefix__29 + ")"
        return t250
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t252 Point = Point{
        x: 3,
        y: 4,
    }
    var t253 string = point32_to_string(t252)
    println__T_string(t253)
    var t254 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t255 string = wrapper_int32_to_string(t254)
    println__T_string(t255)
    var t256 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t257 string = wrapper_unit_to_string(t256)
    println__T_string(t257)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t258 Point = Point{
        x: 3,
        y: 4,
    }
    var t259 Shape__int32 = Shape__int32_Dot{
        _0: t258,
    }
    var t260 string = shape_int32_to_string(t259)
    println__T_string(t260)
    var t261 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t262 Shape__int32 = Shape__int32_Wrapped{
        _0: t261,
    }
    var t263 string = shape_int32_to_string(t262)
    var inline371 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t263)
    _goml_runtime_core_string_println(inline371)
    var t264 string = shape_int32_to_string(bounced_origin__30)
    var inline368 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t264)
    _goml_runtime_core_string_println(inline368)
    var t265 Point = Point{
        x: 3,
        y: 4,
    }
    var t266 Shape__unit = Shape__unit_Dot{
        _0: t265,
    }
    var t267 string = shape_unit_to_string(t266)
    var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t267)
    _goml_runtime_core_string_println(inline365)
    var t268 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t269 Shape__unit = Shape__unit_Wrapped{
        _0: t268,
    }
    var t270 string = shape_unit_to_string(t269)
    var inline362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline362)
    var t271 string
    t271 = "Shape::Origin"
    var inline348 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t271)
    _goml_runtime_core_string_println(inline348)
    var t272 Shape__int32
    t272 = Shape__int32_Origin{}
    switch t272.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline336 string = "struct enums!"
    var inline337 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline336)
    _goml_runtime_core_string_println(inline337)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t276 string = _goml_runtime_core_int32_to_string(self__33)
    return t276
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t279 string = _goml_runtime_core_unit_to_string(self__63)
    return t279
}

func println__T_string(value__1 string) struct{} {
    var t281 string
    t281 = value__1
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
