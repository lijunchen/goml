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
    var retv51 Point
    var t52 Point = Point{
        x: 0,
        y: 0,
    }
    retv51 = t52
    return retv51
}

func flip(point__0 Point) Point {
    var retv54 Point
    var mtmp22 Point = point__0
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__2 int32 = x24
    var x__1 int32 = x23
    var t55 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv54 = t55
    return retv54
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv57 Wrapper__int32
    var t58 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv57 = t58
    return retv57
}

func x_add_1(p__4 Point) Point {
    var retv60 Point
    var mtmp25 Point = p__4
    var x26 int32 = mtmp25.x
    var x27 int32 = mtmp25.y
    var y__6 int32 = x27
    var x__5 int32 = x26
    var t61 int32 = x__5 + 1
    var t62 Point = Point{
        x: t61,
        y: y__6,
    }
    retv60 = t62
    return retv60
}

func point32_to_string(p__13 Point) string {
    var retv68 string
    var mtmp34 Point = p__13
    var x35 int32 = mtmp34.x
    var x36 int32 = mtmp34.y
    var y__15 int32 = x36
    var x__14 int32 = x35
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t70 string = "Point { x: " + t69
    var t71 string = t70 + ", y: "
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t73 string = t71 + t72
    var t74 string = t73 + "}"
    retv68 = t74
    return retv68
}

func point32_to_string2(p__16 Point) string {
    var retv76 string
    var mtmp37 Point = p__16
    var x38 int32 = mtmp37.x
    var x39 int32 = mtmp37.y
    var y__18 int32 = x39
    var x__17 int32 = x38
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t78 string = "Point { x: " + t77
    var t79 string = t78 + ", y: "
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t81 string = t79 + t80
    var t82 string = t81 + "}"
    retv76 = t82
    return retv76
}

func point32_to_string3(p__19 Point) string {
    var retv84 string
    var mtmp40 Point = p__19
    var x41 int32 = mtmp40.x
    var x42 int32 = mtmp40.y
    var y__21 int32 = x42
    var x__20 int32 = x41
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t86 string = "Point { x: " + t85
    var t87 string = t86 + ", y: "
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t89 string = t87 + t88
    var t90 string = t89 + "}"
    retv84 = t90
    return retv84
}

func point32_to_string4(p__22 Point) string {
    var retv92 string
    var mtmp43 Point = p__22
    var x44 int32 = mtmp43.x
    var x45 int32 = mtmp43.y
    var y__24 int32 = x45
    var x__23 int32 = x44
    var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t94 string = "Point { x: " + t93
    var t95 string = t94 + ", y: "
    var t96 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t97 string = t95 + t96
    var t98 string = t97 + "}"
    retv92 = t98
    return retv92
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t100 string = point32_to_string(start__25)
    println__T_string(t100)
    var t101 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t101)
    var t102 string = point32_to_string2(swapped__26)
    println__T_string(t102)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t103 string = point32_to_string3(a__29)
    println__T_string(t103)
    var t104 Point = x_add_1(start__25)
    var a__30 Point = flip(t104)
    var t105 string = point32_to_string4(a__30)
    println__T_string(t105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__2)
    retv107 = t108
    return retv107
}

func println__T_string(value__1 string) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv113 string
    retv113 = self__9
    return retv113
}

func main() {
    main0()
}
