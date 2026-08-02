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
    var retv184 Point
    var t185 Point = Point{
        x: 0,
        y: 0,
    }
    retv184 = t185
    return retv184
}

func flip(point__0 Point) Point {
    var retv187 Point
    var mtmp155 Point = point__0
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__2 int32 = x157
    var x__1 int32 = x156
    var t188 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv187 = t188
    return retv187
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv190 Wrapper__int32
    var t191 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv190 = t191
    return retv190
}

func x_add_1(p__4 Point) Point {
    var retv193 Point
    var mtmp158 Point = p__4
    var x159 int32 = mtmp158.x
    var x160 int32 = mtmp158.y
    var y__6 int32 = x160
    var x__5 int32 = x159
    var t194 int32 = x__5 + 1
    var t195 Point = Point{
        x: t194,
        y: y__6,
    }
    retv193 = t195
    return retv193
}

func point32_to_string(p__13 Point) string {
    var retv201 string
    var mtmp167 Point = p__13
    var x168 int32 = mtmp167.x
    var x169 int32 = mtmp167.y
    var y__15 int32 = x169
    var x__14 int32 = x168
    var t202 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t203 string = "Point { x: " + t202
    var t204 string = t203 + ", y: "
    var t205 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t206 string = t204 + t205
    var t207 string = t206 + "}"
    retv201 = t207
    return retv201
}

func point32_to_string2(p__16 Point) string {
    var retv209 string
    var mtmp170 Point = p__16
    var x171 int32 = mtmp170.x
    var x172 int32 = mtmp170.y
    var y__18 int32 = x172
    var x__17 int32 = x171
    var t210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t211 string = "Point { x: " + t210
    var t212 string = t211 + ", y: "
    var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t214 string = t212 + t213
    var t215 string = t214 + "}"
    retv209 = t215
    return retv209
}

func point32_to_string3(p__19 Point) string {
    var retv217 string
    var mtmp173 Point = p__19
    var x174 int32 = mtmp173.x
    var x175 int32 = mtmp173.y
    var y__21 int32 = x175
    var x__20 int32 = x174
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t219 string = "Point { x: " + t218
    var t220 string = t219 + ", y: "
    var t221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t222 string = t220 + t221
    var t223 string = t222 + "}"
    retv217 = t223
    return retv217
}

func point32_to_string4(p__22 Point) string {
    var retv225 string
    var mtmp176 Point = p__22
    var x177 int32 = mtmp176.x
    var x178 int32 = mtmp176.y
    var y__24 int32 = x178
    var x__23 int32 = x177
    var t226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t227 string = "Point { x: " + t226
    var t228 string = t227 + ", y: "
    var t229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t230 string = t228 + t229
    var t231 string = t230 + "}"
    retv225 = t231
    return retv225
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
    var retv240 string
    var t241 string = _goml_runtime_core_int32_to_string(self__6)
    retv240 = t241
    return retv240
}

func println__T_string(value__1 string) struct{} {
    var t243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv246 string
    retv246 = self__38
    return retv246
}

func main() {
    main0()
}
