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
    var t202 Point = Point{
        x: 0,
        y: 0,
    }
    return t202
}

func flip(point__0 Point) Point {
    var x173 int32 = point__0.x
    var x174 int32 = point__0.y
    var t205 Point = Point{
        x: x174,
        y: x173,
    }
    return t205
}

func x_add_1(p__4 Point) Point {
    var x176 int32 = p__4.x
    var x177 int32 = p__4.y
    var t211 int32 = x176 + 1
    var t212 Point = Point{
        x: t211,
        y: x177,
    }
    return t212
}

func point32_to_string(p__13 Point) string {
    var x185 int32 = p__13.x
    var x186 int32 = p__13.y
    var t219 string
    var inline267 string = _goml_runtime_core_int32_to_string(x185)
    t219 = inline267
    var t220 string = "Point { x: " + t219
    var t221 string = t220 + ", y: "
    var t222 string
    var inline265 string = _goml_runtime_core_int32_to_string(x186)
    t222 = inline265
    var t223 string = t221 + t222
    var t224 string = t223 + "}"
    return t224
}

func point32_to_string2(p__16 Point) string {
    var x188 int32 = p__16.x
    var x189 int32 = p__16.y
    var t227 string
    var inline271 string = _goml_runtime_core_int32_to_string(x188)
    t227 = inline271
    var t228 string = "Point { x: " + t227
    var t229 string = t228 + ", y: "
    var t230 string
    var inline269 string = _goml_runtime_core_int32_to_string(x189)
    t230 = inline269
    var t231 string = t229 + t230
    var t232 string = t231 + "}"
    return t232
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t250 string = point32_to_string(start__25)
    println__T_string(t250)
    var t251 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t251)
    var t252 string = point32_to_string2(swapped__26)
    println__T_string(t252)
    var a__29 Point = x_add_1(start__25)
    var t253 string
    var inline315 int32 = a__29.x
    var inline316 int32 = a__29.y
    var inline319 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline315)
    var inline320 string = "Point { x: " + inline319
    var inline321 string = inline320 + ", y: "
    var inline322 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline316)
    var inline323 string = inline321 + inline322
    var inline324 string = inline323 + "}"
    t253 = inline324
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
    _goml_runtime_core_string_println(inline311)
    var t254 Point
    var inline304 int32 = start__25.x
    var inline305 int32 = start__25.y
    var inline308 int32 = inline304 + 1
    var inline309 Point = Point{
        x: inline308,
        y: inline305,
    }
    t254 = inline309
    var a__30 Point
    var inline297 int32 = t254.x
    var inline298 int32 = t254.y
    var inline301 Point = Point{
        x: inline298,
        y: inline297,
    }
    a__30 = inline301
    var t255 string
    var inline285 int32 = a__30.x
    var inline286 int32 = a__30.y
    var inline289 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline285)
    var inline290 string = "Point { x: " + inline289
    var inline291 string = inline290 + ", y: "
    var inline292 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline286)
    var inline293 string = inline291 + inline292
    var inline294 string = inline293 + "}"
    t255 = inline294
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t255)
    _goml_runtime_core_string_println(inline281)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t258 string = _goml_runtime_core_int32_to_string(self__35)
    return t258
}

func println__T_string(value__31 string) struct{} {
    var t260 string
    t260 = value__31
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
