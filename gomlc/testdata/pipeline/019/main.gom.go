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

type Wrapper__Point struct {
    value Point
}

func make_point() Point {
    var t207 Point = Point{
        x: 0,
        y: 0,
    }
    return t207
}

func flip(point__0 Point) Point {
    var x178 int32 = point__0.x
    var x179 int32 = point__0.y
    var t210 Point = Point{
        x: x179,
        y: x178,
    }
    return t210
}

func x_add_1(p__4 Point) Point {
    var x181 int32 = p__4.x
    var x182 int32 = p__4.y
    var t216 int32 = x181 + 1
    var t217 Point = Point{
        x: t216,
        y: x182,
    }
    return t217
}

func point32_to_string(p__13 Point) string {
    var x190 int32 = p__13.x
    var x191 int32 = p__13.y
    var t224 string
    var inline272 string = _goml_runtime_core_int32_to_string(x190)
    t224 = inline272
    var t225 string = "Point { x: " + t224
    var t226 string = t225 + ", y: "
    var t227 string
    var inline270 string = _goml_runtime_core_int32_to_string(x191)
    t227 = inline270
    var t228 string = t226 + t227
    var t229 string = t228 + "}"
    return t229
}

func point32_to_string2(p__16 Point) string {
    var x193 int32 = p__16.x
    var x194 int32 = p__16.y
    var t232 string
    var inline276 string = _goml_runtime_core_int32_to_string(x193)
    t232 = inline276
    var t233 string = "Point { x: " + t232
    var t234 string = t233 + ", y: "
    var t235 string
    var inline274 string = _goml_runtime_core_int32_to_string(x194)
    t235 = inline274
    var t236 string = t234 + t235
    var t237 string = t236 + "}"
    return t237
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t255 string = point32_to_string(start__25)
    println__T_string(t255)
    var t256 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t256)
    var t257 string = point32_to_string2(swapped__26)
    println__T_string(t257)
    var a__29 Point = x_add_1(start__25)
    var t258 string
    var inline320 int32 = a__29.x
    var inline321 int32 = a__29.y
    var inline324 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline320)
    var inline325 string = "Point { x: " + inline324
    var inline326 string = inline325 + ", y: "
    var inline327 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline321)
    var inline328 string = inline326 + inline327
    var inline329 string = inline328 + "}"
    t258 = inline329
    var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
    _goml_runtime_core_string_println(inline316)
    var t259 Point
    var inline309 int32 = start__25.x
    var inline310 int32 = start__25.y
    var inline313 int32 = inline309 + 1
    var inline314 Point = Point{
        x: inline313,
        y: inline310,
    }
    t259 = inline314
    var a__30 Point
    var inline302 int32 = t259.x
    var inline303 int32 = t259.y
    var inline306 Point = Point{
        x: inline303,
        y: inline302,
    }
    a__30 = inline306
    var t260 string
    var inline290 int32 = a__30.x
    var inline291 int32 = a__30.y
    var inline294 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline290)
    var inline295 string = "Point { x: " + inline294
    var inline296 string = inline295 + ", y: "
    var inline297 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline291)
    var inline298 string = inline296 + inline297
    var inline299 string = inline298 + "}"
    t260 = inline299
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline286)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t263 string = _goml_runtime_core_int32_to_string(self__35)
    return t263
}

func println__T_string(value__31 string) struct{} {
    var t265 string
    t265 = value__31
    _goml_runtime_core_string_println(t265)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
