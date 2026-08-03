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
    var t166 Point = Point{
        x: 0,
        y: 0,
    }
    return t166
}

func flip(point__0 Point) Point {
    var x137 int32 = point__0.x
    var x138 int32 = point__0.y
    var t169 Point = Point{
        x: x138,
        y: x137,
    }
    return t169
}

func x_add_1(p__4 Point) Point {
    var x140 int32 = p__4.x
    var x141 int32 = p__4.y
    var t175 int32 = x140 + 1
    var t176 Point = Point{
        x: t175,
        y: x141,
    }
    return t176
}

func point32_to_string(p__13 Point) string {
    var x149 int32 = p__13.x
    var x150 int32 = p__13.y
    var t183 string
    var inline231 string = _goml_runtime_core_int32_to_string(x149)
    t183 = inline231
    var t184 string = "Point { x: " + t183
    var t185 string = t184 + ", y: "
    var t186 string
    var inline229 string = _goml_runtime_core_int32_to_string(x150)
    t186 = inline229
    var t187 string = t185 + t186
    var t188 string = t187 + "}"
    return t188
}

func point32_to_string2(p__16 Point) string {
    var x152 int32 = p__16.x
    var x153 int32 = p__16.y
    var t191 string
    var inline235 string = _goml_runtime_core_int32_to_string(x152)
    t191 = inline235
    var t192 string = "Point { x: " + t191
    var t193 string = t192 + ", y: "
    var t194 string
    var inline233 string = _goml_runtime_core_int32_to_string(x153)
    t194 = inline233
    var t195 string = t193 + t194
    var t196 string = t195 + "}"
    return t196
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t214 string = point32_to_string(start__25)
    println__T_string(t214)
    var t215 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t215)
    var t216 string = point32_to_string2(swapped__26)
    println__T_string(t216)
    var a__29 Point = x_add_1(start__25)
    var t217 string
    var inline279 int32 = a__29.x
    var inline280 int32 = a__29.y
    var inline283 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline279)
    var inline284 string = "Point { x: " + inline283
    var inline285 string = inline284 + ", y: "
    var inline286 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline280)
    var inline287 string = inline285 + inline286
    var inline288 string = inline287 + "}"
    t217 = inline288
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline275)
    var t218 Point
    var inline268 int32 = start__25.x
    var inline269 int32 = start__25.y
    var inline272 int32 = inline268 + 1
    var inline273 Point = Point{
        x: inline272,
        y: inline269,
    }
    t218 = inline273
    var a__30 Point
    var inline261 int32 = t218.x
    var inline262 int32 = t218.y
    var inline265 Point = Point{
        x: inline262,
        y: inline261,
    }
    a__30 = inline265
    var t219 string
    var inline249 int32 = a__30.x
    var inline250 int32 = a__30.y
    var inline253 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline249)
    var inline254 string = "Point { x: " + inline253
    var inline255 string = inline254 + ", y: "
    var inline256 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline250)
    var inline257 string = inline255 + inline256
    var inline258 string = inline257 + "}"
    t219 = inline258
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__35)
    return t222
}

func println__T_string(value__31 string) struct{} {
    var t224 string
    t224 = value__31
    _goml_runtime_core_string_println(t224)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
