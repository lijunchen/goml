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
    var retv90 Point
    var t91 Point = Point{
        x: 0,
        y: 0,
    }
    retv90 = t91
    return retv90
}

func flip(point__0 Point) Point {
    var retv93 Point
    var mtmp61 Point = point__0
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__2 int32 = x63
    var x__1 int32 = x62
    var t94 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv93 = t94
    return retv93
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv96 Wrapper__int32
    var t97 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv96 = t97
    return retv96
}

func x_add_1(p__4 Point) Point {
    var retv99 Point
    var mtmp64 Point = p__4
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__6 int32 = x66
    var x__5 int32 = x65
    var t100 int32 = x__5 + 1
    var t101 Point = Point{
        x: t100,
        y: y__6,
    }
    retv99 = t101
    return retv99
}

func point32_to_string(p__13 Point) string {
    var retv107 string
    var mtmp73 Point = p__13
    var x74 int32 = mtmp73.x
    var x75 int32 = mtmp73.y
    var y__15 int32 = x75
    var x__14 int32 = x74
    var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t109 string = "Point { x: " + t108
    var t110 string = t109 + ", y: "
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t112 string = t110 + t111
    var t113 string = t112 + "}"
    retv107 = t113
    return retv107
}

func point32_to_string2(p__16 Point) string {
    var retv115 string
    var mtmp76 Point = p__16
    var x77 int32 = mtmp76.x
    var x78 int32 = mtmp76.y
    var y__18 int32 = x78
    var x__17 int32 = x77
    var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t117 string = "Point { x: " + t116
    var t118 string = t117 + ", y: "
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t120 string = t118 + t119
    var t121 string = t120 + "}"
    retv115 = t121
    return retv115
}

func point32_to_string3(p__19 Point) string {
    var retv123 string
    var mtmp79 Point = p__19
    var x80 int32 = mtmp79.x
    var x81 int32 = mtmp79.y
    var y__21 int32 = x81
    var x__20 int32 = x80
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t125 string = "Point { x: " + t124
    var t126 string = t125 + ", y: "
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t128 string = t126 + t127
    var t129 string = t128 + "}"
    retv123 = t129
    return retv123
}

func point32_to_string4(p__22 Point) string {
    var retv131 string
    var mtmp82 Point = p__22
    var x83 int32 = mtmp82.x
    var x84 int32 = mtmp82.y
    var y__24 int32 = x84
    var x__23 int32 = x83
    var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t133 string = "Point { x: " + t132
    var t134 string = t133 + ", y: "
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t136 string = t134 + t135
    var t137 string = t136 + "}"
    retv131 = t137
    return retv131
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t139 string = point32_to_string(start__25)
    println__T_string(t139)
    var t140 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t140)
    var t141 string = point32_to_string2(swapped__26)
    println__T_string(t141)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t142 string = point32_to_string3(a__29)
    println__T_string(t142)
    var t143 Point = x_add_1(start__25)
    var a__30 Point = flip(t143)
    var t144 string = point32_to_string4(a__30)
    println__T_string(t144)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv146 string
    var t147 string = _goml_runtime_core_int32_to_string(self__5)
    retv146 = t147
    return retv146
}

func println__T_string(value__1 string) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv152 string
    retv152 = self__37
    return retv152
}

func main() {
    main0()
}
