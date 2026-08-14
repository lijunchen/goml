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
    var t217 Point = Point{
        x: 0,
        y: 0,
    }
    return t217
}

func flip(point__0 Point) Point {
    var x188 int32 = point__0.x
    var x189 int32 = point__0.y
    var t220 Point = Point{
        x: x189,
        y: x188,
    }
    return t220
}

func x_add_1(p__4 Point) Point {
    var x191 int32 = p__4.x
    var x192 int32 = p__4.y
    var t226 int32 = x191 + 1
    var t227 Point = Point{
        x: t226,
        y: x192,
    }
    return t227
}

func point32_to_string(p__13 Point) string {
    var x200 int32 = p__13.x
    var x201 int32 = p__13.y
    var t234 string
    var inline282 string = _goml_runtime_core_int32_to_string(x200)
    t234 = inline282
    var t235 string = "Point { x: " + t234
    var t236 string = t235 + ", y: "
    var t237 string
    var inline280 string = _goml_runtime_core_int32_to_string(x201)
    t237 = inline280
    var t238 string = t236 + t237
    var t239 string = t238 + "}"
    return t239
}

func point32_to_string2(p__16 Point) string {
    var x203 int32 = p__16.x
    var x204 int32 = p__16.y
    var t242 string
    var inline286 string = _goml_runtime_core_int32_to_string(x203)
    t242 = inline286
    var t243 string = "Point { x: " + t242
    var t244 string = t243 + ", y: "
    var t245 string
    var inline284 string = _goml_runtime_core_int32_to_string(x204)
    t245 = inline284
    var t246 string = t244 + t245
    var t247 string = t246 + "}"
    return t247
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t265 string = point32_to_string(start__25)
    println__T_string(t265)
    var t266 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t266)
    var t267 string = point32_to_string2(swapped__26)
    println__T_string(t267)
    var a__29 Point = x_add_1(start__25)
    var t268 string
    var inline330 int32 = a__29.x
    var inline331 int32 = a__29.y
    var inline334 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline330)
    var inline335 string = "Point { x: " + inline334
    var inline336 string = inline335 + ", y: "
    var inline337 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline331)
    var inline338 string = inline336 + inline337
    var inline339 string = inline338 + "}"
    t268 = inline339
    var inline326 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t268)
    _goml_runtime_core_string_println(inline326)
    var t269 Point
    var inline319 int32 = start__25.x
    var inline320 int32 = start__25.y
    var inline323 int32 = inline319 + 1
    var inline324 Point = Point{
        x: inline323,
        y: inline320,
    }
    t269 = inline324
    var a__30 Point
    var inline312 int32 = t269.x
    var inline313 int32 = t269.y
    var inline316 Point = Point{
        x: inline313,
        y: inline312,
    }
    a__30 = inline316
    var t270 string
    var inline300 int32 = a__30.x
    var inline301 int32 = a__30.y
    var inline304 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline300)
    var inline305 string = "Point { x: " + inline304
    var inline306 string = inline305 + ", y: "
    var inline307 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline301)
    var inline308 string = inline306 + inline307
    var inline309 string = inline308 + "}"
    t270 = inline309
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline296)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t273 string = _goml_runtime_core_int32_to_string(self__33)
    return t273
}

func println__T_string(value__1 string) struct{} {
    var t275 string
    t275 = value__1
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
