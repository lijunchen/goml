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
    var retv93 Point
    var t94 Point = Point{
        x: 0,
        y: 0,
    }
    retv93 = t94
    return retv93
}

func flip(point__0 Point) Point {
    var retv96 Point
    var mtmp64 Point = point__0
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__2 int32 = x66
    var x__1 int32 = x65
    var t97 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv96 = t97
    return retv96
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv99 Wrapper__int32
    var t100 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv99 = t100
    return retv99
}

func x_add_1(p__4 Point) Point {
    var retv102 Point
    var mtmp67 Point = p__4
    var x68 int32 = mtmp67.x
    var x69 int32 = mtmp67.y
    var y__6 int32 = x69
    var x__5 int32 = x68
    var t103 int32 = x__5 + 1
    var t104 Point = Point{
        x: t103,
        y: y__6,
    }
    retv102 = t104
    return retv102
}

func point32_to_string(p__13 Point) string {
    var retv110 string
    var mtmp76 Point = p__13
    var x77 int32 = mtmp76.x
    var x78 int32 = mtmp76.y
    var y__15 int32 = x78
    var x__14 int32 = x77
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t112 string = "Point { x: " + t111
    var t113 string = t112 + ", y: "
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t115 string = t113 + t114
    var t116 string = t115 + "}"
    retv110 = t116
    return retv110
}

func point32_to_string2(p__16 Point) string {
    var retv118 string
    var mtmp79 Point = p__16
    var x80 int32 = mtmp79.x
    var x81 int32 = mtmp79.y
    var y__18 int32 = x81
    var x__17 int32 = x80
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t120 string = "Point { x: " + t119
    var t121 string = t120 + ", y: "
    var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t123 string = t121 + t122
    var t124 string = t123 + "}"
    retv118 = t124
    return retv118
}

func point32_to_string3(p__19 Point) string {
    var retv126 string
    var mtmp82 Point = p__19
    var x83 int32 = mtmp82.x
    var x84 int32 = mtmp82.y
    var y__21 int32 = x84
    var x__20 int32 = x83
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t128 string = "Point { x: " + t127
    var t129 string = t128 + ", y: "
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t131 string = t129 + t130
    var t132 string = t131 + "}"
    retv126 = t132
    return retv126
}

func point32_to_string4(p__22 Point) string {
    var retv134 string
    var mtmp85 Point = p__22
    var x86 int32 = mtmp85.x
    var x87 int32 = mtmp85.y
    var y__24 int32 = x87
    var x__23 int32 = x86
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t136 string = "Point { x: " + t135
    var t137 string = t136 + ", y: "
    var t138 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t139 string = t137 + t138
    var t140 string = t139 + "}"
    retv134 = t140
    return retv134
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t142 string = point32_to_string(start__25)
    println__T_string(t142)
    var t143 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t143)
    var t144 string = point32_to_string2(swapped__26)
    println__T_string(t144)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t145 string = point32_to_string3(a__29)
    println__T_string(t145)
    var t146 Point = x_add_1(start__25)
    var a__30 Point = flip(t146)
    var t147 string = point32_to_string4(a__30)
    println__T_string(t147)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv149 string
    var t150 string = _goml_runtime_core_int32_to_string(self__6)
    retv149 = t150
    return retv149
}

func println__T_string(value__1 string) struct{} {
    var t152 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t152)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv155 string
    retv155 = self__38
    return retv155
}

func main() {
    main0()
}
