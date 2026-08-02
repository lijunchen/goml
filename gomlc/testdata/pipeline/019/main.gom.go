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
    var t185 Point = Point{
        x: 0,
        y: 0,
    }
    return t185
}

func flip(point__0 Point) Point {
    var x156 int32 = point__0.x
    var x157 int32 = point__0.y
    var t188 Point = Point{
        x: x157,
        y: x156,
    }
    return t188
}

func x_add_1(p__4 Point) Point {
    var x159 int32 = p__4.x
    var x160 int32 = p__4.y
    var t194 int32 = x159 + 1
    var t195 Point = Point{
        x: t194,
        y: x160,
    }
    return t195
}

func point32_to_string(p__13 Point) string {
    var x168 int32 = p__13.x
    var x169 int32 = p__13.y
    var t202 string
    var inline250 string = _goml_runtime_core_int32_to_string(x168)
    t202 = inline250
    var t203 string = "Point { x: " + t202
    var t204 string = t203 + ", y: "
    var t205 string
    var inline248 string = _goml_runtime_core_int32_to_string(x169)
    t205 = inline248
    var t206 string = t204 + t205
    var t207 string = t206 + "}"
    return t207
}

func point32_to_string2(p__16 Point) string {
    var x171 int32 = p__16.x
    var x172 int32 = p__16.y
    var t210 string
    var inline254 string = _goml_runtime_core_int32_to_string(x171)
    t210 = inline254
    var t211 string = "Point { x: " + t210
    var t212 string = t211 + ", y: "
    var t213 string
    var inline252 string = _goml_runtime_core_int32_to_string(x172)
    t213 = inline252
    var t214 string = t212 + t213
    var t215 string = t214 + "}"
    return t215
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t233 string = point32_to_string(start__25)
    println__T_string(t233)
    var t234 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t234)
    var t235 string = point32_to_string2(swapped__26)
    println__T_string(t235)
    var a__29 Point = x_add_1(start__25)
    var t236 string
    var inline298 int32 = a__29.x
    var inline299 int32 = a__29.y
    var inline302 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline298)
    var inline303 string = "Point { x: " + inline302
    var inline304 string = inline303 + ", y: "
    var inline305 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline299)
    var inline306 string = inline304 + inline305
    var inline307 string = inline306 + "}"
    t236 = inline307
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline294)
    var t237 Point
    var inline287 int32 = start__25.x
    var inline288 int32 = start__25.y
    var inline291 int32 = inline287 + 1
    var inline292 Point = Point{
        x: inline291,
        y: inline288,
    }
    t237 = inline292
    var a__30 Point
    var inline280 int32 = t237.x
    var inline281 int32 = t237.y
    var inline284 Point = Point{
        x: inline281,
        y: inline280,
    }
    a__30 = inline284
    var t238 string
    var inline268 int32 = a__30.x
    var inline269 int32 = a__30.y
    var inline272 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline268)
    var inline273 string = "Point { x: " + inline272
    var inline274 string = inline273 + ", y: "
    var inline275 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline269)
    var inline276 string = inline274 + inline275
    var inline277 string = inline276 + "}"
    t238 = inline277
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t241 string = _goml_runtime_core_int32_to_string(self__6)
    return t241
}

func println__T_string(value__1 string) struct{} {
    var t243 string
    t243 = value__1
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
