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
    var t28 Point
    t28 = Point{
        x: 0,
        y: 0,
    }
    return t28
}

func flip(point__0 Point) Point {
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var t29 Point
    mtmp0 = point__0
    x1 = mtmp0.x
    x2 = mtmp0.y
    y__2 = x2
    x__1 = x1
    t29 = Point{
        x: y__2,
        y: x__1,
    }
    return t29
}

func wrap_int(x__3 int32) Wrapper__int32 {
    var t30 Wrapper__int32
    t30 = Wrapper__int32{
        value: x__3,
    }
    return t30
}

func x_add_1(p__4 Point) Point {
    var mtmp3 Point
    var x4 int32
    var x5 int32
    var y__6 int32
    var x__5 int32
    var t31 int32
    var t32 Point
    mtmp3 = p__4
    x4 = mtmp3.x
    x5 = mtmp3.y
    y__6 = x5
    x__5 = x4
    t31 = x__5 + 1
    t32 = Point{
        x: t31,
        y: y__6,
    }
    return t32
}

func point32_to_string(p__13 Point) string {
    var mtmp12 Point
    var x13 int32
    var x14 int32
    var y__15 int32
    var x__14 int32
    var t33 string
    var t34 string
    var t35 string
    var t36 string
    var t37 string
    var t38 string
    mtmp12 = p__13
    x13 = mtmp12.x
    x14 = mtmp12.y
    y__15 = x14
    x__14 = x13
    t33 = int32_to_string(x__14)
    t34 = "Point { x: " + t33
    t35 = t34 + ", y: "
    t36 = int32_to_string(y__15)
    t37 = t35 + t36
    t38 = t37 + "}"
    return t38
}

func point32_to_string2(p__16 Point) string {
    var mtmp15 Point
    var x16 int32
    var x17 int32
    var y__18 int32
    var x__17 int32
    var t39 string
    var t40 string
    var t41 string
    var t42 string
    var t43 string
    var t44 string
    mtmp15 = p__16
    x16 = mtmp15.x
    x17 = mtmp15.y
    y__18 = x17
    x__17 = x16
    t39 = int32_to_string(x__17)
    t40 = "Point { x: " + t39
    t41 = t40 + ", y: "
    t42 = int32_to_string(y__18)
    t43 = t41 + t42
    t44 = t43 + "}"
    return t44
}

func point32_to_string3(p__19 Point) string {
    var mtmp18 Point
    var x19 int32
    var x20 int32
    var y__21 int32
    var x__20 int32
    var t45 string
    var t46 string
    var t47 string
    var t48 string
    var t49 string
    var t50 string
    mtmp18 = p__19
    x19 = mtmp18.x
    x20 = mtmp18.y
    y__21 = x20
    x__20 = x19
    t45 = int32_to_string(x__20)
    t46 = "Point { x: " + t45
    t47 = t46 + ", y: "
    t48 = int32_to_string(y__21)
    t49 = t47 + t48
    t50 = t49 + "}"
    return t50
}

func point32_to_string4(p__22 Point) string {
    var mtmp21 Point
    var x22 int32
    var x23 int32
    var y__24 int32
    var x__23 int32
    var t51 string
    var t52 string
    var t53 string
    var t54 string
    var t55 string
    var t56 string
    mtmp21 = p__22
    x22 = mtmp21.x
    x23 = mtmp21.y
    y__24 = x23
    x__23 = x22
    t51 = int32_to_string(x__23)
    t52 = "Point { x: " + t51
    t53 = t52 + ", y: "
    t54 = int32_to_string(y__24)
    t55 = t53 + t54
    t56 = t55 + "}"
    return t56
}

func main0() struct{} {
    var start__25 Point
    var t57 string
    var t58 Point
    var swapped__26 Point
    var t59 string
    var a__29 Point
    var t60 string
    var t61 Point
    var a__30 Point
    var t62 string
    start__25 = make_point()
    t57 = point32_to_string(start__25)
    string_println(t57)
    t58 = Point{
        x: 1,
        y: 2,
    }
    swapped__26 = flip(t58)
    t59 = point32_to_string2(swapped__26)
    string_println(t59)
    wrap_int(3)
    a__29 = x_add_1(start__25)
    t60 = point32_to_string3(a__29)
    string_println(t60)
    t61 = x_add_1(start__25)
    a__30 = flip(t61)
    t62 = point32_to_string4(a__30)
    string_println(t62)
    return struct{}{}
}

func main() {
    main0()
}
