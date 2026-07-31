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
    var retv181 Point
    var t182 Point = Point{
        x: 0,
        y: 0,
    }
    retv181 = t182
    return retv181
}

func flip(point__0 Point) Point {
    var retv184 Point
    var mtmp152 Point = point__0
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var y__2 int32 = x154
    var x__1 int32 = x153
    var t185 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv184 = t185
    return retv184
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv187 Wrapper__int32
    var t188 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv187 = t188
    return retv187
}

func x_add_1(p__4 Point) Point {
    var retv190 Point
    var mtmp155 Point = p__4
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__6 int32 = x157
    var x__5 int32 = x156
    var t191 int32 = x__5 + 1
    var t192 Point = Point{
        x: t191,
        y: y__6,
    }
    retv190 = t192
    return retv190
}

func point32_to_string(p__13 Point) string {
    var retv198 string
    var mtmp164 Point = p__13
    var x165 int32 = mtmp164.x
    var x166 int32 = mtmp164.y
    var y__15 int32 = x166
    var x__14 int32 = x165
    var t199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t200 string = "Point { x: " + t199
    var t201 string = t200 + ", y: "
    var t202 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t203 string = t201 + t202
    var t204 string = t203 + "}"
    retv198 = t204
    return retv198
}

func point32_to_string2(p__16 Point) string {
    var retv206 string
    var mtmp167 Point = p__16
    var x168 int32 = mtmp167.x
    var x169 int32 = mtmp167.y
    var y__18 int32 = x169
    var x__17 int32 = x168
    var t207 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t208 string = "Point { x: " + t207
    var t209 string = t208 + ", y: "
    var t210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t211 string = t209 + t210
    var t212 string = t211 + "}"
    retv206 = t212
    return retv206
}

func point32_to_string3(p__19 Point) string {
    var retv214 string
    var mtmp170 Point = p__19
    var x171 int32 = mtmp170.x
    var x172 int32 = mtmp170.y
    var y__21 int32 = x172
    var x__20 int32 = x171
    var t215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t216 string = "Point { x: " + t215
    var t217 string = t216 + ", y: "
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t219 string = t217 + t218
    var t220 string = t219 + "}"
    retv214 = t220
    return retv214
}

func point32_to_string4(p__22 Point) string {
    var retv222 string
    var mtmp173 Point = p__22
    var x174 int32 = mtmp173.x
    var x175 int32 = mtmp173.y
    var y__24 int32 = x175
    var x__23 int32 = x174
    var t223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t224 string = "Point { x: " + t223
    var t225 string = t224 + ", y: "
    var t226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t227 string = t225 + t226
    var t228 string = t227 + "}"
    retv222 = t228
    return retv222
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t230 string = point32_to_string(start__25)
    println__T_string(t230)
    var t231 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t231)
    var t232 string = point32_to_string2(swapped__26)
    println__T_string(t232)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t233 string = point32_to_string3(a__29)
    println__T_string(t233)
    var t234 Point = x_add_1(start__25)
    var a__30 Point = flip(t234)
    var t235 string = point32_to_string4(a__30)
    println__T_string(t235)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv237 string
    var t238 string = _goml_runtime_core_int32_to_string(self__6)
    retv237 = t238
    return retv237
}

func println__T_string(value__1 string) struct{} {
    var t240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv243 string
    retv243 = self__38
    return retv243
}

func main() {
    main0()
}
