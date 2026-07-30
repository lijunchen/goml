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
    var retv97 Point
    var t98 Point = Point{
        x: 0,
        y: 0,
    }
    retv97 = t98
    return retv97
}

func flip(point__0 Point) Point {
    var retv100 Point
    var mtmp68 Point = point__0
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__2 int32 = x70
    var x__1 int32 = x69
    var t101 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv100 = t101
    return retv100
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv103 Wrapper__int32
    var t104 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv103 = t104
    return retv103
}

func x_add_1(p__4 Point) Point {
    var retv106 Point
    var mtmp71 Point = p__4
    var x72 int32 = mtmp71.x
    var x73 int32 = mtmp71.y
    var y__6 int32 = x73
    var x__5 int32 = x72
    var t107 int32 = x__5 + 1
    var t108 Point = Point{
        x: t107,
        y: y__6,
    }
    retv106 = t108
    return retv106
}

func point32_to_string(p__13 Point) string {
    var retv114 string
    var mtmp80 Point = p__13
    var x81 int32 = mtmp80.x
    var x82 int32 = mtmp80.y
    var y__15 int32 = x82
    var x__14 int32 = x81
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t116 string = "Point { x: " + t115
    var t117 string = t116 + ", y: "
    var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t119 string = t117 + t118
    var t120 string = t119 + "}"
    retv114 = t120
    return retv114
}

func point32_to_string2(p__16 Point) string {
    var retv122 string
    var mtmp83 Point = p__16
    var x84 int32 = mtmp83.x
    var x85 int32 = mtmp83.y
    var y__18 int32 = x85
    var x__17 int32 = x84
    var t123 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t124 string = "Point { x: " + t123
    var t125 string = t124 + ", y: "
    var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t127 string = t125 + t126
    var t128 string = t127 + "}"
    retv122 = t128
    return retv122
}

func point32_to_string3(p__19 Point) string {
    var retv130 string
    var mtmp86 Point = p__19
    var x87 int32 = mtmp86.x
    var x88 int32 = mtmp86.y
    var y__21 int32 = x88
    var x__20 int32 = x87
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t132 string = "Point { x: " + t131
    var t133 string = t132 + ", y: "
    var t134 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t135 string = t133 + t134
    var t136 string = t135 + "}"
    retv130 = t136
    return retv130
}

func point32_to_string4(p__22 Point) string {
    var retv138 string
    var mtmp89 Point = p__22
    var x90 int32 = mtmp89.x
    var x91 int32 = mtmp89.y
    var y__24 int32 = x91
    var x__23 int32 = x90
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t140 string = "Point { x: " + t139
    var t141 string = t140 + ", y: "
    var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t143 string = t141 + t142
    var t144 string = t143 + "}"
    retv138 = t144
    return retv138
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t146 string = point32_to_string(start__25)
    println__T_string(t146)
    var t147 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t147)
    var t148 string = point32_to_string2(swapped__26)
    println__T_string(t148)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t149 string = point32_to_string3(a__29)
    println__T_string(t149)
    var t150 Point = x_add_1(start__25)
    var a__30 Point = flip(t150)
    var t151 string = point32_to_string4(a__30)
    println__T_string(t151)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv153 string
    var t154 string = _goml_runtime_core_int32_to_string(self__6)
    retv153 = t154
    return retv153
}

func println__T_string(value__1 string) struct{} {
    var t156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv159 string
    retv159 = self__38
    return retv159
}

func main() {
    main0()
}
