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
    var retv36 Point
    var t37 Point = Point{
        x: 0,
        y: 0,
    }
    retv36 = t37
    return retv36
}

func flip(point__0 Point) Point {
    var retv39 Point
    var mtmp7 Point = point__0
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__2 int32 = x9
    var x__1 int32 = x8
    var t40 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv39 = t40
    return retv39
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv42 Wrapper__int32
    var t43 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv42 = t43
    return retv42
}

func x_add_1(p__4 Point) Point {
    var retv45 Point
    var mtmp10 Point = p__4
    var x11 int32 = mtmp10.x
    var x12 int32 = mtmp10.y
    var y__6 int32 = x12
    var x__5 int32 = x11
    var t46 int32 = x__5 + 1
    var t47 Point = Point{
        x: t46,
        y: y__6,
    }
    retv45 = t47
    return retv45
}

func point32_to_string(p__13 Point) string {
    var retv53 string
    var mtmp19 Point = p__13
    var x20 int32 = mtmp19.x
    var x21 int32 = mtmp19.y
    var y__15 int32 = x21
    var x__14 int32 = x20
    var t54 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__14)
    var t55 string = "Point { x: " + t54
    var t56 string = t55 + ", y: "
    var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__15)
    var t58 string = t56 + t57
    var t59 string = t58 + "}"
    retv53 = t59
    return retv53
}

func point32_to_string2(p__16 Point) string {
    var retv61 string
    var mtmp22 Point = p__16
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__18 int32 = x24
    var x__17 int32 = x23
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__17)
    var t63 string = "Point { x: " + t62
    var t64 string = t63 + ", y: "
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__18)
    var t66 string = t64 + t65
    var t67 string = t66 + "}"
    retv61 = t67
    return retv61
}

func point32_to_string3(p__19 Point) string {
    var retv69 string
    var mtmp25 Point = p__19
    var x26 int32 = mtmp25.x
    var x27 int32 = mtmp25.y
    var y__21 int32 = x27
    var x__20 int32 = x26
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__20)
    var t71 string = "Point { x: " + t70
    var t72 string = t71 + ", y: "
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__21)
    var t74 string = t72 + t73
    var t75 string = t74 + "}"
    retv69 = t75
    return retv69
}

func point32_to_string4(p__22 Point) string {
    var retv77 string
    var mtmp28 Point = p__22
    var x29 int32 = mtmp28.x
    var x30 int32 = mtmp28.y
    var y__24 int32 = x30
    var x__23 int32 = x29
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
    var t79 string = "Point { x: " + t78
    var t80 string = t79 + ", y: "
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
    var t82 string = t80 + t81
    var t83 string = t82 + "}"
    retv77 = t83
    return retv77
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t85 string = point32_to_string(start__25)
    println__T_string(t85)
    var t86 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t86)
    var t87 string = point32_to_string2(swapped__26)
    println__T_string(t87)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t88 string = point32_to_string3(a__29)
    println__T_string(t88)
    var t89 Point = x_add_1(start__25)
    var a__30 Point = flip(t89)
    var t90 string = point32_to_string4(a__30)
    println__T_string(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int32_to_string(self__2)
    retv92 = t93
    return retv92
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv98 string
    retv98 = self__9
    return retv98
}

func main() {
    main0()
}
