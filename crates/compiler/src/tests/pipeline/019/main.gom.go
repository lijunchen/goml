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
    var retv33 Point
    var t34 Point = Point{
        x: 0,
        y: 0,
    }
    retv33 = t34
    return retv33
}

func flip(point__0 Point) Point {
    var retv36 Point
    var mtmp4 Point = point__0
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__2 int32 = x6
    var x__1 int32 = x5
    var t37 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv36 = t37
    return retv36
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv39 Wrapper__int32
    var t40 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv39 = t40
    return retv39
}

func x_add_1(p__4 Point) Point {
    var retv42 Point
    var mtmp7 Point = p__4
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__6 int32 = x9
    var x__5 int32 = x8
    var t43 int32 = x__5 + 1
    var t44 Point = Point{
        x: t43,
        y: y__6,
    }
    retv42 = t44
    return retv42
}

func point32_to_string(p__13 Point) string {
    var retv50 string
    var mtmp16 Point = p__13
    var x17 int32 = mtmp16.x
    var x18 int32 = mtmp16.y
    var y__15 int32 = x18
    var x__14 int32 = x17
    var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t52 string = "Point { x: " + t51
    var t53 string = t52 + ", y: "
    var t54 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t55 string = t53 + t54
    var t56 string = t55 + "}"
    retv50 = t56
    return retv50
}

func point32_to_string2(p__16 Point) string {
    var retv58 string
    var mtmp19 Point = p__16
    var x20 int32 = mtmp19.x
    var x21 int32 = mtmp19.y
    var y__18 int32 = x21
    var x__17 int32 = x20
    var t59 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t60 string = "Point { x: " + t59
    var t61 string = t60 + ", y: "
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t63 string = t61 + t62
    var t64 string = t63 + "}"
    retv58 = t64
    return retv58
}

func point32_to_string3(p__19 Point) string {
    var retv66 string
    var mtmp22 Point = p__19
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__21 int32 = x24
    var x__20 int32 = x23
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t68 string = "Point { x: " + t67
    var t69 string = t68 + ", y: "
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t71 string = t69 + t70
    var t72 string = t71 + "}"
    retv66 = t72
    return retv66
}

func point32_to_string4(p__22 Point) string {
    var retv74 string
    var mtmp25 Point = p__22
    var x26 int32 = mtmp25.x
    var x27 int32 = mtmp25.y
    var y__24 int32 = x27
    var x__23 int32 = x26
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t76 string = "Point { x: " + t75
    var t77 string = t76 + ", y: "
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t79 string = t77 + t78
    var t80 string = t79 + "}"
    retv74 = t80
    return retv74
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t82 string = point32_to_string(start__25)
    println__T_string(t82)
    var t83 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t83)
    var t84 string = point32_to_string2(swapped__26)
    println__T_string(t84)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t85 string = point32_to_string3(a__29)
    println__T_string(t85)
    var t86 Point = x_add_1(start__25)
    var a__30 Point = flip(t86)
    var t87 string = point32_to_string4(a__30)
    println__T_string(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__2)
    retv89 = t90
    return retv89
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv95 string
    retv95 = self__9
    return retv95
}

func main() {
    main0()
}
