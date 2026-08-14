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
        var x187 Point = shape__0.(Shape__int32_Dot)._0
        var t216 Shape__int32 = Shape__int32_Dot{
            _0: x187,
        }
        return t216
    case Shape__int32_Wrapped:
        var x188 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t217 Shape__int32 = Shape__int32_Wrapped{
            _0: x188,
        }
        return t217
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x192 int32 = point__8.x
    var x193 int32 = point__8.y
    var t229 string
    var inline297 string = _goml_runtime_core_int32_to_string(x192)
    t229 = inline297
    var with_x__11 string = "Point { x: " + t229
    var with_y_label__12 string = with_x__11 + ", y: "
    var t230 string
    var inline295 string = _goml_runtime_core_int32_to_string(x193)
    t230 = inline295
    var with_y__13 string = with_y_label__12 + t230
    var t231 string = with_y__13 + " }"
    return t231
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x195 int32 = wrapper__14.value
    var t234 string
    var inline299 string = _goml_runtime_core_int32_to_string(x195)
    t234 = inline299
    var prefix__16 string = "Wrapper[int32] { value: " + t234
    var t235 string = prefix__16 + " }"
    return t235
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x197 struct{} = wrapper__17.value
    var t238 string
    var inline301 string = _goml_runtime_core_unit_to_string(x197)
    t238 = inline301
    var prefix__19 string = "Wrapper[unit] { value: " + t238
    var t239 string = prefix__19 + " }"
    return t239
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x198 Point = shape__20.(Shape__int32_Dot)._0
        var t244 string
        var inline304 int32 = x198.x
        var inline305 int32 = x198.y
        var inline308 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline304)
        var inline309 string = "Point { x: " + inline308
        var inline310 string = inline309 + ", y: "
        var inline311 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline305)
        var inline312 string = inline310 + inline311
        var inline313 string = inline312 + " }"
        t244 = inline313
        var prefix__22 string = "Shape::Dot(" + t244
        var t245 string = prefix__22 + ")"
        return t245
    case Shape__int32_Wrapped:
        var x199 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t246 string
        var inline316 int32 = x199.value
        var inline318 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline316)
        var inline319 string = "Wrapper[int32] { value: " + inline318
        var inline320 string = inline319 + " }"
        t246 = inline320
        var prefix__24 string = "Shape::Wrapped(" + t246
        var t247 string = prefix__24 + ")"
        return t247
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x200 Point = shape__25.(Shape__unit_Dot)._0
        var t252 string
        var inline323 int32 = x200.x
        var inline324 int32 = x200.y
        var inline327 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline323)
        var inline328 string = "Point { x: " + inline327
        var inline329 string = inline328 + ", y: "
        var inline330 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline324)
        var inline331 string = inline329 + inline330
        var inline332 string = inline331 + " }"
        t252 = inline332
        var prefix__27 string = "Shape::Dot(" + t252
        var t253 string = prefix__27 + ")"
        return t253
    case Shape__unit_Wrapped:
        var x201 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t254 string
        var inline335 struct{} = x201.value
        var inline337 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline335)
        var inline338 string = "Wrapper[unit] { value: " + inline337
        var inline339 string = inline338 + " }"
        t254 = inline339
        var prefix__29 string = "Shape::Wrapped(" + t254
        var t255 string = prefix__29 + ")"
        return t255
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t257 Point = Point{
        x: 3,
        y: 4,
    }
    var t258 string = point32_to_string(t257)
    println__T_string(t258)
    var t259 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t260 string = wrapper_int32_to_string(t259)
    println__T_string(t260)
    var t261 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t262 string = wrapper_unit_to_string(t261)
    println__T_string(t262)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t263 Point = Point{
        x: 3,
        y: 4,
    }
    var t264 Shape__int32 = Shape__int32_Dot{
        _0: t263,
    }
    var t265 string = shape_int32_to_string(t264)
    println__T_string(t265)
    var t266 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t267 Shape__int32 = Shape__int32_Wrapped{
        _0: t266,
    }
    var t268 string = shape_int32_to_string(t267)
    var inline376 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t268)
    _goml_runtime_core_string_println(inline376)
    var t269 string = shape_int32_to_string(bounced_origin__30)
    var inline373 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t269)
    _goml_runtime_core_string_println(inline373)
    var t270 Point = Point{
        x: 3,
        y: 4,
    }
    var t271 Shape__unit = Shape__unit_Dot{
        _0: t270,
    }
    var t272 string = shape_unit_to_string(t271)
    var inline370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t272)
    _goml_runtime_core_string_println(inline370)
    var t273 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t274 Shape__unit = Shape__unit_Wrapped{
        _0: t273,
    }
    var t275 string = shape_unit_to_string(t274)
    var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t275)
    _goml_runtime_core_string_println(inline367)
    var t276 string
    t276 = "Shape::Origin"
    var inline353 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t276)
    _goml_runtime_core_string_println(inline353)
    var t277 Shape__int32
    t277 = Shape__int32_Origin{}
    switch t277.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline341 string = "struct enums!"
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline341)
    _goml_runtime_core_string_println(inline342)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t281 string = _goml_runtime_core_int32_to_string(self__33)
    return t281
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t284 string = _goml_runtime_core_unit_to_string(self__63)
    return t284
}

func println__T_string(value__1 string) struct{} {
    var t286 string
    t286 = value__1
    _goml_runtime_core_string_println(t286)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
