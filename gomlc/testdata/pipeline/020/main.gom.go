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
        var x177 Point = shape__0.(Shape__int32_Dot)._0
        var t206 Shape__int32 = Shape__int32_Dot{
            _0: x177,
        }
        return t206
    case Shape__int32_Wrapped:
        var x178 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t207 Shape__int32 = Shape__int32_Wrapped{
            _0: x178,
        }
        return t207
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x182 int32 = point__8.x
    var x183 int32 = point__8.y
    var t219 string
    var inline287 string = _goml_runtime_core_int32_to_string(x182)
    t219 = inline287
    var with_x__11 string = "Point { x: " + t219
    var with_y_label__12 string = with_x__11 + ", y: "
    var t220 string
    var inline285 string = _goml_runtime_core_int32_to_string(x183)
    t220 = inline285
    var with_y__13 string = with_y_label__12 + t220
    var t221 string = with_y__13 + " }"
    return t221
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x185 int32 = wrapper__14.value
    var t224 string
    var inline289 string = _goml_runtime_core_int32_to_string(x185)
    t224 = inline289
    var prefix__16 string = "Wrapper[int32] { value: " + t224
    var t225 string = prefix__16 + " }"
    return t225
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x187 struct{} = wrapper__17.value
    var t228 string
    var inline291 string = _goml_runtime_core_unit_to_string(x187)
    t228 = inline291
    var prefix__19 string = "Wrapper[unit] { value: " + t228
    var t229 string = prefix__19 + " }"
    return t229
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x188 Point = shape__20.(Shape__int32_Dot)._0
        var t234 string
        var inline294 int32 = x188.x
        var inline295 int32 = x188.y
        var inline298 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline294)
        var inline299 string = "Point { x: " + inline298
        var inline300 string = inline299 + ", y: "
        var inline301 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline295)
        var inline302 string = inline300 + inline301
        var inline303 string = inline302 + " }"
        t234 = inline303
        var prefix__22 string = "Shape::Dot(" + t234
        var t235 string = prefix__22 + ")"
        return t235
    case Shape__int32_Wrapped:
        var x189 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t236 string
        var inline306 int32 = x189.value
        var inline308 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline306)
        var inline309 string = "Wrapper[int32] { value: " + inline308
        var inline310 string = inline309 + " }"
        t236 = inline310
        var prefix__24 string = "Shape::Wrapped(" + t236
        var t237 string = prefix__24 + ")"
        return t237
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x190 Point = shape__25.(Shape__unit_Dot)._0
        var t242 string
        var inline313 int32 = x190.x
        var inline314 int32 = x190.y
        var inline317 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline313)
        var inline318 string = "Point { x: " + inline317
        var inline319 string = inline318 + ", y: "
        var inline320 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline314)
        var inline321 string = inline319 + inline320
        var inline322 string = inline321 + " }"
        t242 = inline322
        var prefix__27 string = "Shape::Dot(" + t242
        var t243 string = prefix__27 + ")"
        return t243
    case Shape__unit_Wrapped:
        var x191 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t244 string
        var inline325 struct{} = x191.value
        var inline327 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline325)
        var inline328 string = "Wrapper[unit] { value: " + inline327
        var inline329 string = inline328 + " }"
        t244 = inline329
        var prefix__29 string = "Shape::Wrapped(" + t244
        var t245 string = prefix__29 + ")"
        return t245
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t247 Point = Point{
        x: 3,
        y: 4,
    }
    var t248 string = point32_to_string(t247)
    println__T_string(t248)
    var t249 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t250 string = wrapper_int32_to_string(t249)
    println__T_string(t250)
    var t251 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t252 string = wrapper_unit_to_string(t251)
    println__T_string(t252)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t253 Point = Point{
        x: 3,
        y: 4,
    }
    var t254 Shape__int32 = Shape__int32_Dot{
        _0: t253,
    }
    var t255 string = shape_int32_to_string(t254)
    println__T_string(t255)
    var t256 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t257 Shape__int32 = Shape__int32_Wrapped{
        _0: t256,
    }
    var t258 string = shape_int32_to_string(t257)
    var inline368 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
    _goml_runtime_core_string_println(inline368)
    var t259 string = shape_int32_to_string(bounced_origin__30)
    var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t259)
    _goml_runtime_core_string_println(inline365)
    var t260 Point = Point{
        x: 3,
        y: 4,
    }
    var t261 Shape__unit = Shape__unit_Dot{
        _0: t260,
    }
    var t262 string = shape_unit_to_string(t261)
    var inline362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline362)
    var t263 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t264 Shape__unit = Shape__unit_Wrapped{
        _0: t263,
    }
    var t265 string = shape_unit_to_string(t264)
    var inline359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline359)
    var t266 string
    t266 = "Shape::Origin"
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t266)
    _goml_runtime_core_string_println(inline345)
    var t267 Shape__int32
    t267 = Shape__int32_Origin{}
    switch t267.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline331 string = "struct enums!"
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline331)
    _goml_runtime_core_string_println(inline332)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t271 string = _goml_runtime_core_int32_to_string(self__35)
    return t271
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t274 string = _goml_runtime_core_unit_to_string(self__65)
    return t274
}

func println__T_string(value__31 string) struct{} {
    var t276 string
    t276 = value__31
    _goml_runtime_core_string_println(t276)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
