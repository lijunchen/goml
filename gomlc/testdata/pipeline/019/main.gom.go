package main

import (
    _goml_fmt "fmt"
)

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

func make_point() Point {
    var t212 Point = Point{
        x: 0,
        y: 0,
    }
    return t212
}

func flip(point__0 Point) Point {
    var x183 int32 = point__0.x
    var x184 int32 = point__0.y
    var t215 Point = Point{
        x: x184,
        y: x183,
    }
    return t215
}

func x_add_1(p__4 Point) Point {
    var x186 int32 = p__4.x
    var x187 int32 = p__4.y
    var t221 int32 = x186 + 1
    var t222 Point = Point{
        x: t221,
        y: x187,
    }
    return t222
}

func point32_to_string(p__13 Point) string {
    var x195 int32 = p__13.x
    var x196 int32 = p__13.y
    var t229 string
    var inline277 string = _goml_runtime_core_int32_to_string(x195)
    t229 = inline277
    var t230 string = "Point { x: " + t229
    var t231 string = t230 + ", y: "
    var t232 string
    var inline275 string = _goml_runtime_core_int32_to_string(x196)
    t232 = inline275
    var t233 string = t231 + t232
    var t234 string = t233 + "}"
    return t234
}

func point32_to_string2(p__16 Point) string {
    var x198 int32 = p__16.x
    var x199 int32 = p__16.y
    var t237 string
    var inline281 string = _goml_runtime_core_int32_to_string(x198)
    t237 = inline281
    var t238 string = "Point { x: " + t237
    var t239 string = t238 + ", y: "
    var t240 string
    var inline279 string = _goml_runtime_core_int32_to_string(x199)
    t240 = inline279
    var t241 string = t239 + t240
    var t242 string = t241 + "}"
    return t242
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t260 string = point32_to_string(start__25)
    println__T_string(t260)
    var t261 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t261)
    var t262 string = point32_to_string2(swapped__26)
    println__T_string(t262)
    var a__29 Point = x_add_1(start__25)
    var t263 string
    var inline325 int32 = a__29.x
    var inline326 int32 = a__29.y
    var inline329 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline325)
    var inline330 string = "Point { x: " + inline329
    var inline331 string = inline330 + ", y: "
    var inline332 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline326)
    var inline333 string = inline331 + inline332
    var inline334 string = inline333 + "}"
    t263 = inline334
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t263)
    _goml_runtime_core_string_println(inline321)
    var t264 Point
    var inline314 int32 = start__25.x
    var inline315 int32 = start__25.y
    var inline318 int32 = inline314 + 1
    var inline319 Point = Point{
        x: inline318,
        y: inline315,
    }
    t264 = inline319
    var a__30 Point
    var inline307 int32 = t264.x
    var inline308 int32 = t264.y
    var inline311 Point = Point{
        x: inline308,
        y: inline307,
    }
    a__30 = inline311
    var t265 string
    var inline295 int32 = a__30.x
    var inline296 int32 = a__30.y
    var inline299 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline295)
    var inline300 string = "Point { x: " + inline299
    var inline301 string = inline300 + ", y: "
    var inline302 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline296)
    var inline303 string = inline301 + inline302
    var inline304 string = inline303 + "}"
    t265 = inline304
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline291)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t268 string = _goml_runtime_core_int32_to_string(self__33)
    return t268
}

func println__T_string(value__1 string) struct{} {
    var t270 string
    t270 = value__1
    _goml_runtime_core_string_println(t270)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
