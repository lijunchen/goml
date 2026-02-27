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
    var ret66 Point
    ret66 = Point{
        x: 0,
        y: 0,
    }
    return ret66
}

func flip(point__0 Point) Point {
    var ret67 Point
    var mtmp0 Point = point__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    ret67 = Point{
        x: y__2,
        y: x__1,
    }
    return ret67
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var ret68 Wrapper__int32
    ret68 = Wrapper__int32{
        value: x__3,
    }
    return ret68
}

func x_add_1(p__4 Point) Point {
    var ret69 Point
    var mtmp3 Point = p__4
    var x4 int32 = mtmp3.x
    var x5 int32 = mtmp3.y
    var y__6 int32 = x5
    var x__5 int32 = x4
    var t39 int32 = x__5 + 1
    ret69 = Point{
        x: t39,
        y: y__6,
    }
    return ret69
}

func point32_to_string(p__13 Point) string {
    var ret72 string
    var mtmp12 Point = p__13
    var x13 int32 = mtmp12.x
    var x14 int32 = mtmp12.y
    var y__15 int32 = x14
    var x__14 int32 = x13
    var t43 string = int32_to_string(x__14)
    var t42 string = "Point { x: " + t43
    var t41 string = t42 + ", y: "
    var t44 string = int32_to_string(y__15)
    var t40 string = t41 + t44
    ret72 = t40 + "}"
    return ret72
}

func point32_to_string2(p__16 Point) string {
    var ret73 string
    var mtmp15 Point = p__16
    var x16 int32 = mtmp15.x
    var x17 int32 = mtmp15.y
    var y__18 int32 = x17
    var x__17 int32 = x16
    var t48 string = int32_to_string(x__17)
    var t47 string = "Point { x: " + t48
    var t46 string = t47 + ", y: "
    var t49 string = int32_to_string(y__18)
    var t45 string = t46 + t49
    ret73 = t45 + "}"
    return ret73
}

func point32_to_string3(p__19 Point) string {
    var ret74 string
    var mtmp18 Point = p__19
    var x19 int32 = mtmp18.x
    var x20 int32 = mtmp18.y
    var y__21 int32 = x20
    var x__20 int32 = x19
    var t53 string = int32_to_string(x__20)
    var t52 string = "Point { x: " + t53
    var t51 string = t52 + ", y: "
    var t54 string = int32_to_string(y__21)
    var t50 string = t51 + t54
    ret74 = t50 + "}"
    return ret74
}

func point32_to_string4(p__22 Point) string {
    var ret75 string
    var mtmp21 Point = p__22
    var x22 int32 = mtmp21.x
    var x23 int32 = mtmp21.y
    var y__24 int32 = x23
    var x__23 int32 = x22
    var t58 string = int32_to_string(x__23)
    var t57 string = "Point { x: " + t58
    var t56 string = t57 + ", y: "
    var t59 string = int32_to_string(y__24)
    var t55 string = t56 + t59
    ret75 = t55 + "}"
    return ret75
}

func main0() struct{} {
    var ret76 struct{}
    var start__25 Point = make_point()
    var t60 string = point32_to_string(start__25)
    string_println(t60)
    var t61 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t61)
    var t62 string = point32_to_string2(swapped__26)
    string_println(t62)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t63 string = point32_to_string3(a__29)
    string_println(t63)
    var t64 Point = x_add_1(start__25)
    var a__30 Point = flip(t64)
    var t65 string = point32_to_string4(a__30)
    string_println(t65)
    ret76 = struct{}{}
    return ret76
}

func main() {
    main0()
}
