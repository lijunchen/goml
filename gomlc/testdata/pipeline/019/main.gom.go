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
    var retv137 Point
    var t138 Point = Point{
        x: 0,
        y: 0,
    }
    retv137 = t138
    return retv137
}

func flip(point__0 Point) Point {
    var retv140 Point
    var mtmp108 Point = point__0
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var y__2 int32 = x110
    var x__1 int32 = x109
    var t141 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv140 = t141
    return retv140
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv143 Wrapper__int32
    var t144 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv143 = t144
    return retv143
}

func x_add_1(p__4 Point) Point {
    var retv146 Point
    var mtmp111 Point = p__4
    var x112 int32 = mtmp111.x
    var x113 int32 = mtmp111.y
    var y__6 int32 = x113
    var x__5 int32 = x112
    var t147 int32 = x__5 + 1
    var t148 Point = Point{
        x: t147,
        y: y__6,
    }
    retv146 = t148
    return retv146
}

func point32_to_string(p__13 Point) string {
    var retv154 string
    var mtmp120 Point = p__13
    var x121 int32 = mtmp120.x
    var x122 int32 = mtmp120.y
    var y__15 int32 = x122
    var x__14 int32 = x121
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t156 string = "Point { x: " + t155
    var t157 string = t156 + ", y: "
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t159 string = t157 + t158
    var t160 string = t159 + "}"
    retv154 = t160
    return retv154
}

func point32_to_string2(p__16 Point) string {
    var retv162 string
    var mtmp123 Point = p__16
    var x124 int32 = mtmp123.x
    var x125 int32 = mtmp123.y
    var y__18 int32 = x125
    var x__17 int32 = x124
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t164 string = "Point { x: " + t163
    var t165 string = t164 + ", y: "
    var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t167 string = t165 + t166
    var t168 string = t167 + "}"
    retv162 = t168
    return retv162
}

func point32_to_string3(p__19 Point) string {
    var retv170 string
    var mtmp126 Point = p__19
    var x127 int32 = mtmp126.x
    var x128 int32 = mtmp126.y
    var y__21 int32 = x128
    var x__20 int32 = x127
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t172 string = "Point { x: " + t171
    var t173 string = t172 + ", y: "
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t175 string = t173 + t174
    var t176 string = t175 + "}"
    retv170 = t176
    return retv170
}

func point32_to_string4(p__22 Point) string {
    var retv178 string
    var mtmp129 Point = p__22
    var x130 int32 = mtmp129.x
    var x131 int32 = mtmp129.y
    var y__24 int32 = x131
    var x__23 int32 = x130
    var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t180 string = "Point { x: " + t179
    var t181 string = t180 + ", y: "
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t183 string = t181 + t182
    var t184 string = t183 + "}"
    retv178 = t184
    return retv178
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t186 string = point32_to_string(start__25)
    println__T_string(t186)
    var t187 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t187)
    var t188 string = point32_to_string2(swapped__26)
    println__T_string(t188)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t189 string = point32_to_string3(a__29)
    println__T_string(t189)
    var t190 Point = x_add_1(start__25)
    var a__30 Point = flip(t190)
    var t191 string = point32_to_string4(a__30)
    println__T_string(t191)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv193 string
    var t194 string = _goml_runtime_core_int32_to_string(self__6)
    retv193 = t194
    return retv193
}

func println__T_string(value__1 string) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv199 string
    retv199 = self__38
    return retv199
}

func main() {
    main0()
}
