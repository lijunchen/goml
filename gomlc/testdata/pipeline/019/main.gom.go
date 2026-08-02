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

func wrap_int(x__3 int32) Wrapper__int32 {
    var t191 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    return t191
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
    var t202 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x168)
    var t203 string = "Point { x: " + t202
    var t204 string = t203 + ", y: "
    var t205 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x169)
    var t206 string = t204 + t205
    var t207 string = t206 + "}"
    return t207
}

func point32_to_string2(p__16 Point) string {
    var x171 int32 = p__16.x
    var x172 int32 = p__16.y
    var t210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x171)
    var t211 string = "Point { x: " + t210
    var t212 string = t211 + ", y: "
    var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x172)
    var t214 string = t212 + t213
    var t215 string = t214 + "}"
    return t215
}

func point32_to_string3(p__19 Point) string {
    var x174 int32 = p__19.x
    var x175 int32 = p__19.y
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x174)
    var t219 string = "Point { x: " + t218
    var t220 string = t219 + ", y: "
    var t221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x175)
    var t222 string = t220 + t221
    var t223 string = t222 + "}"
    return t223
}

func point32_to_string4(p__22 Point) string {
    var x177 int32 = p__22.x
    var x178 int32 = p__22.y
    var t226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x177)
    var t227 string = "Point { x: " + t226
    var t228 string = t227 + ", y: "
    var t229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x178)
    var t230 string = t228 + t229
    var t231 string = t230 + "}"
    return t231
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
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t236 string = point32_to_string3(a__29)
    println__T_string(t236)
    var t237 Point = x_add_1(start__25)
    var a__30 Point = flip(t237)
    var t238 string = point32_to_string4(a__30)
    println__T_string(t238)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t241 string = _goml_runtime_core_int32_to_string(self__6)
    return t241
}

func println__T_string(value__1 string) struct{} {
    var t243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
