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
        var x172 Point = shape__0.(Shape__int32_Dot)._0
        var t201 Shape__int32 = Shape__int32_Dot{
            _0: x172,
        }
        return t201
    case Shape__int32_Wrapped:
        var x173 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t202 Shape__int32 = Shape__int32_Wrapped{
            _0: x173,
        }
        return t202
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x177 int32 = point__8.x
    var x178 int32 = point__8.y
    var t214 string
    var inline282 string = _goml_runtime_core_int32_to_string(x177)
    t214 = inline282
    var with_x__11 string = "Point { x: " + t214
    var with_y_label__12 string = with_x__11 + ", y: "
    var t215 string
    var inline280 string = _goml_runtime_core_int32_to_string(x178)
    t215 = inline280
    var with_y__13 string = with_y_label__12 + t215
    var t216 string = with_y__13 + " }"
    return t216
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x180 int32 = wrapper__14.value
    var t219 string
    var inline284 string = _goml_runtime_core_int32_to_string(x180)
    t219 = inline284
    var prefix__16 string = "Wrapper[int32] { value: " + t219
    var t220 string = prefix__16 + " }"
    return t220
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x182 struct{} = wrapper__17.value
    var t223 string
    var inline286 string = _goml_runtime_core_unit_to_string(x182)
    t223 = inline286
    var prefix__19 string = "Wrapper[unit] { value: " + t223
    var t224 string = prefix__19 + " }"
    return t224
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x183 Point = shape__20.(Shape__int32_Dot)._0
        var t229 string
        var inline289 int32 = x183.x
        var inline290 int32 = x183.y
        var inline293 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline289)
        var inline294 string = "Point { x: " + inline293
        var inline295 string = inline294 + ", y: "
        var inline296 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline290)
        var inline297 string = inline295 + inline296
        var inline298 string = inline297 + " }"
        t229 = inline298
        var prefix__22 string = "Shape::Dot(" + t229
        var t230 string = prefix__22 + ")"
        return t230
    case Shape__int32_Wrapped:
        var x184 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t231 string
        var inline301 int32 = x184.value
        var inline303 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline301)
        var inline304 string = "Wrapper[int32] { value: " + inline303
        var inline305 string = inline304 + " }"
        t231 = inline305
        var prefix__24 string = "Shape::Wrapped(" + t231
        var t232 string = prefix__24 + ")"
        return t232
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x185 Point = shape__25.(Shape__unit_Dot)._0
        var t237 string
        var inline308 int32 = x185.x
        var inline309 int32 = x185.y
        var inline312 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline308)
        var inline313 string = "Point { x: " + inline312
        var inline314 string = inline313 + ", y: "
        var inline315 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline309)
        var inline316 string = inline314 + inline315
        var inline317 string = inline316 + " }"
        t237 = inline317
        var prefix__27 string = "Shape::Dot(" + t237
        var t238 string = prefix__27 + ")"
        return t238
    case Shape__unit_Wrapped:
        var x186 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t239 string
        var inline320 struct{} = x186.value
        var inline322 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline320)
        var inline323 string = "Wrapper[unit] { value: " + inline322
        var inline324 string = inline323 + " }"
        t239 = inline324
        var prefix__29 string = "Shape::Wrapped(" + t239
        var t240 string = prefix__29 + ")"
        return t240
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t242 Point = Point{
        x: 3,
        y: 4,
    }
    var t243 string = point32_to_string(t242)
    println__T_string(t243)
    var t244 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t245 string = wrapper_int32_to_string(t244)
    println__T_string(t245)
    var t246 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t247 string = wrapper_unit_to_string(t246)
    println__T_string(t247)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t248 Point = Point{
        x: 3,
        y: 4,
    }
    var t249 Shape__int32 = Shape__int32_Dot{
        _0: t248,
    }
    var t250 string = shape_int32_to_string(t249)
    println__T_string(t250)
    var t251 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t252 Shape__int32 = Shape__int32_Wrapped{
        _0: t251,
    }
    var t253 string = shape_int32_to_string(t252)
    var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
    _goml_runtime_core_string_println(inline361)
    var t254 string = shape_int32_to_string(bounced_origin__30)
    var inline358 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline358)
    var t255 Point = Point{
        x: 3,
        y: 4,
    }
    var t256 Shape__unit = Shape__unit_Dot{
        _0: t255,
    }
    var t257 string = shape_unit_to_string(t256)
    var inline355 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline355)
    var t258 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t259 Shape__unit = Shape__unit_Wrapped{
        _0: t258,
    }
    var t260 string = shape_unit_to_string(t259)
    var inline352 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline352)
    var t261 string
    t261 = "Shape::Origin"
    var inline338 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t261)
    _goml_runtime_core_string_println(inline338)
    var t262 Shape__int32
    t262 = Shape__int32_Origin{}
    switch t262.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline326 string = "struct enums!"
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline326)
    _goml_runtime_core_string_println(inline327)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t266 string = _goml_runtime_core_int32_to_string(self__33)
    return t266
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t269 string = _goml_runtime_core_unit_to_string(self__63)
    return t269
}

func println__T_string(value__1 string) struct{} {
    var t271 string
    t271 = value__1
    _goml_runtime_core_string_println(t271)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
