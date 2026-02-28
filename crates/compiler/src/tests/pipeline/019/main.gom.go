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
    var t28 Point = Point{
        x: 0,
        y: 0,
    }
    return t28
}

func flip(point__0 Point) Point {
    var mtmp0 Point = point__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t29 Point = Point{
        x: y__2,
        y: x__1,
    }
    return t29
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var t30 Wrapper__int32 = Wrapper__int32{
        value: x__3,
    }
    return t30
}

func x_add_1(p__4 Point) Point {
    var mtmp3 Point = p__4
    var x4 int32 = mtmp3.x
    var x5 int32 = mtmp3.y
    var y__6 int32 = x5
    var x__5 int32 = x4
    var t31 int32 = x__5 + 1
    var t32 Point = Point{
        x: t31,
        y: y__6,
    }
    return t32
}

func point32_to_string(p__13 Point) string {
    var mtmp12 Point = p__13
    var x13 int32 = mtmp12.x
    var x14 int32 = mtmp12.y
    var y__15 int32 = x14
    var x__14 int32 = x13
    var t33 string = int32_to_string(x__14)
    var t34 string = "Point { x: " + t33
    var t35 string = t34 + ", y: "
    var t36 string = int32_to_string(y__15)
    var t37 string = t35 + t36
    var t38 string = t37 + "}"
    return t38
}

func point32_to_string2(p__16 Point) string {
    var mtmp15 Point = p__16
    var x16 int32 = mtmp15.x
    var x17 int32 = mtmp15.y
    var y__18 int32 = x17
    var x__17 int32 = x16
    var t39 string = int32_to_string(x__17)
    var t40 string = "Point { x: " + t39
    var t41 string = t40 + ", y: "
    var t42 string = int32_to_string(y__18)
    var t43 string = t41 + t42
    var t44 string = t43 + "}"
    return t44
}

func point32_to_string3(p__19 Point) string {
    var mtmp18 Point = p__19
    var x19 int32 = mtmp18.x
    var x20 int32 = mtmp18.y
    var y__21 int32 = x20
    var x__20 int32 = x19
    var t45 string = int32_to_string(x__20)
    var t46 string = "Point { x: " + t45
    var t47 string = t46 + ", y: "
    var t48 string = int32_to_string(y__21)
    var t49 string = t47 + t48
    var t50 string = t49 + "}"
    return t50
}

func point32_to_string4(p__22 Point) string {
    var mtmp21 Point = p__22
    var x22 int32 = mtmp21.x
    var x23 int32 = mtmp21.y
    var y__24 int32 = x23
    var x__23 int32 = x22
    var t51 string = int32_to_string(x__23)
    var t52 string = "Point { x: " + t51
    var t53 string = t52 + ", y: "
    var t54 string = int32_to_string(y__24)
    var t55 string = t53 + t54
    var t56 string = t55 + "}"
    return t56
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t57 string = point32_to_string(start__25)
    string_println(t57)
    var t58 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t58)
    var t59 string = point32_to_string2(swapped__26)
    string_println(t59)
    wrap_int(3)
    var a__29 Point = x_add_1(start__25)
    var t60 string = point32_to_string3(a__29)
    string_println(t60)
    var t61 Point = x_add_1(start__25)
    var a__30 Point = flip(t61)
    var t62 string = point32_to_string4(a__30)
    string_println(t62)
    return struct{}{}
}

func main() {
    main0()
}
