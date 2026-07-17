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
    var retv87 Point
    var t88 Point = Point{
        x: 0,
        y: 0,
    }
    retv87 = t88
    return retv87
}

func flip(point__0 Point) Point {
    var retv90 Point
    var mtmp58 Point = point__0
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var y__2 int32 = x60
    var x__1 int32 = x59
    var t91 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv90 = t91
    return retv90
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv93 Wrapper__int32
    var t94 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv93 = t94
    return retv93
}

func x_add_1(p__4 Point) Point {
    var retv96 Point
    var mtmp61 Point = p__4
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__6 int32 = x63
    var x__5 int32 = x62
    var t97 int32 = x__5 + 1
    var t98 Point = Point{
        x: t97,
        y: y__6,
    }
    retv96 = t98
    return retv96
}

func point32_to_string(p__13 Point) string {
    var retv104 string
    var mtmp70 Point = p__13
    var x71 int32 = mtmp70.x
    var x72 int32 = mtmp70.y
    var y__15 int32 = x72
    var x__14 int32 = x71
    var t105 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t106 string = "Point { x: " + t105
    var t107 string = t106 + ", y: "
    var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t109 string = t107 + t108
    var t110 string = t109 + "}"
    retv104 = t110
    return retv104
}

func point32_to_string2(p__16 Point) string {
    var retv112 string
    var mtmp73 Point = p__16
    var x74 int32 = mtmp73.x
    var x75 int32 = mtmp73.y
    var y__18 int32 = x75
    var x__17 int32 = x74
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t114 string = "Point { x: " + t113
    var t115 string = t114 + ", y: "
    var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t117 string = t115 + t116
    var t118 string = t117 + "}"
    retv112 = t118
    return retv112
}

func point32_to_string3(p__19 Point) string {
    var retv120 string
    var mtmp76 Point = p__19
    var x77 int32 = mtmp76.x
    var x78 int32 = mtmp76.y
    var y__21 int32 = x78
    var x__20 int32 = x77
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t122 string = "Point { x: " + t121
    var t123 string = t122 + ", y: "
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t125 string = t123 + t124
    var t126 string = t125 + "}"
    retv120 = t126
    return retv120
}

func point32_to_string4(p__22 Point) string {
    var retv128 string
    var mtmp79 Point = p__22
    var x80 int32 = mtmp79.x
    var x81 int32 = mtmp79.y
    var y__24 int32 = x81
    var x__23 int32 = x80
    var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t130 string = "Point { x: " + t129
    var t131 string = t130 + ", y: "
    var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t133 string = t131 + t132
    var t134 string = t133 + "}"
    retv128 = t134
    return retv128
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t136 string = point32_to_string(start__25)
    println__T_string(t136)
    var t137 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t137)
    var t138 string = point32_to_string2(swapped__26)
    println__T_string(t138)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t139 string = point32_to_string3(a__29)
    println__T_string(t139)
    var t140 Point = x_add_1(start__25)
    var a__30 Point = flip(t140)
    var t141 string = point32_to_string4(a__30)
    println__T_string(t141)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_int32_to_string(self__2)
    retv143 = t144
    return retv143
}

func println__T_string(value__1 string) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv149 string
    retv149 = self__34
    return retv149
}

func main() {
    main0()
}
