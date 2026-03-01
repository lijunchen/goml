package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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
    var retv29 Point
    var t30 Point = Point{
        x: 0,
        y: 0,
    }
    retv29 = t30
    return retv29
}

func flip(point__0 Point) Point {
    var retv32 Point
    var mtmp0 Point = point__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t33 Point = Point{
        x: y__2,
        y: x__1,
    }
    retv32 = t33
    return retv32
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var retv35 Wrapper__int32
    var t36 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    retv35 = t36
    return retv35
}

func x_add_1(p__4 Point) Point {
    var retv38 Point
    var mtmp3 Point = p__4
    var x4 int32 = mtmp3.x
    var x5 int32 = mtmp3.y
    var y__6 int32 = x5
    var x__5 int32 = x4
    var t39 int32 = x__5 + 1
    var t40 Point = Point{
        x: t39,
        y: y__6,
    }
    retv38 = t40
    return retv38
}

func point32_to_string(p__13 Point) string {
    var retv46 string
    var mtmp12 Point = p__13
    var x13 int32 = mtmp12.x
    var x14 int32 = mtmp12.y
    var y__15 int32 = x14
    var x__14 int32 = x13
    var t47 string = int32_to_string(x__14)
    var t48 string = "Point { x: " + t47
    var t49 string = t48 + ", y: "
    var t50 string = int32_to_string(y__15)
    var t51 string = t49 + t50
    var t52 string = t51 + "}"
    retv46 = t52
    return retv46
}

func point32_to_string2(p__16 Point) string {
    var retv54 string
    var mtmp15 Point = p__16
    var x16 int32 = mtmp15.x
    var x17 int32 = mtmp15.y
    var y__18 int32 = x17
    var x__17 int32 = x16
    var t55 string = int32_to_string(x__17)
    var t56 string = "Point { x: " + t55
    var t57 string = t56 + ", y: "
    var t58 string = int32_to_string(y__18)
    var t59 string = t57 + t58
    var t60 string = t59 + "}"
    retv54 = t60
    return retv54
}

func point32_to_string3(p__19 Point) string {
    var retv62 string
    var mtmp18 Point = p__19
    var x19 int32 = mtmp18.x
    var x20 int32 = mtmp18.y
    var y__21 int32 = x20
    var x__20 int32 = x19
    var t63 string = int32_to_string(x__20)
    var t64 string = "Point { x: " + t63
    var t65 string = t64 + ", y: "
    var t66 string = int32_to_string(y__21)
    var t67 string = t65 + t66
    var t68 string = t67 + "}"
    retv62 = t68
    return retv62
}

func point32_to_string4(p__22 Point) string {
    var retv70 string
    var mtmp21 Point = p__22
    var x22 int32 = mtmp21.x
    var x23 int32 = mtmp21.y
    var y__24 int32 = x23
    var x__23 int32 = x22
    var t71 string = int32_to_string(x__23)
    var t72 string = "Point { x: " + t71
    var t73 string = t72 + ", y: "
    var t74 string = int32_to_string(y__24)
    var t75 string = t73 + t74
    var t76 string = t75 + "}"
    retv70 = t76
    return retv70
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t78 string = point32_to_string(start__25)
    string_println(t78)
    var t79 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t79)
    var t80 string = point32_to_string2(swapped__26)
    string_println(t80)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t81 string = point32_to_string3(a__29)
    string_println(t81)
    var t82 Point = x_add_1(start__25)
    var a__30 Point = flip(t82)
    var t83 string = point32_to_string4(a__30)
    string_println(t83)
    return struct{}{}
}

func main() {
    main0()
}
