package main

import (
    "fmt"
)

func bool_to_json(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func json_escape_string(s string) string {
    return fmt.Sprintf("%q", s)
}

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

type Person struct {
    name string
    age int32
    active bool
}

type Color interface {
    isColor()
}

type Red struct {}

func (_ Red) isColor() {}

type Green struct {}

func (_ Green) isColor() {}

type Blue struct {}

func (_ Blue) isColor() {}

type Rgb struct {
    _0 int32
    _1 int32
    _2 int32
}

func (_ Rgb) isColor() {}

func goml__inherent_x23_Point_x23_Point_x23_to__json(self__0 Point) string {
    var ret44 string
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t18 string = "{" + "\"x\":"
    var t19 string = int32_to_string(x__1)
    var t17 string = t18 + t19
    var t16 string = t17 + ","
    var t15 string = t16 + "\"y\":"
    var t20 string = int32_to_string(y__2)
    var t14 string = t15 + t20
    ret44 = t14 + "}"
    return ret44
}

func goml__inherent_x23_Person_x23_Person_x23_to__json(self__3 Person) string {
    var ret45 string
    var mtmp3 Person = self__3
    var x4 string = mtmp3.name
    var x5 int32 = mtmp3.age
    var x6 bool = mtmp3.active
    var active__6 bool = x6
    var age__5 int32 = x5
    var name__4 string = x4
    var t28 string = "{" + "\"name\":"
    var t29 string = json_escape_string(name__4)
    var t27 string = t28 + t29
    var t26 string = t27 + ","
    var t25 string = t26 + "\"age\":"
    var t30 string = int32_to_string(age__5)
    var t24 string = t25 + t30
    var t23 string = t24 + ","
    var t22 string = t23 + "\"active\":"
    var t31 string = bool_to_json(active__6)
    var t21 string = t22 + t31
    ret45 = t21 + "}"
    return ret45
}

func goml__inherent_x23_Color_x23_Color_x23_to__json(self__7 Color) string {
    var ret46 string
    switch self__7 := self__7.(type) {
    case Red:
        ret46 = "{\"tag\":\"Red\"}"
    case Green:
        ret46 = "{\"tag\":\"Green\"}"
    case Blue:
        ret46 = "{\"tag\":\"Blue\"}"
    case Rgb:
        var x7 int32 = self__7._0
        var x8 int32 = self__7._1
        var x9 int32 = self__7._2
        var __field2__10 int32 = x9
        var __field1__9 int32 = x8
        var __field0__8 int32 = x7
        var t37 string = int32_to_string(__field0__8)
        var t36 string = "{\"tag\":\"Rgb\",\"fields\":[" + t37
        var t35 string = t36 + ","
        var t38 string = int32_to_string(__field1__9)
        var t34 string = t35 + t38
        var t33 string = t34 + ","
        var t39 string = int32_to_string(__field2__10)
        var t32 string = t33 + t39
        ret46 = t32 + "]}"
    }
    return ret46
}

func main0() struct{} {
    var ret47 struct{}
    var p__11 Point = Point{
        x: 10,
        y: 20,
    }
    var person__12 Person = Person{
        name: "Alice",
        age: 30,
        active: true,
    }
    var c1__13 Color = Red{}
    var c2__14 Color = Rgb{
        _0: 255,
        _1: 128,
        _2: 0,
    }
    var t40 string = goml__inherent_x23_Point_x23_Point_x23_to__json(p__11)
    string_println(t40)
    var t41 string = goml__inherent_x23_Person_x23_Person_x23_to__json(person__12)
    string_println(t41)
    var t42 string = goml__inherent_x23_Color_x23_Color_x23_to__json(c1__13)
    string_println(t42)
    var t43 string = goml__inherent_x23_Color_x23_Color_x23_to__json(c2__14)
    string_println(t43)
    ret47 = struct{}{}
    return ret47
}

func main() {
    main0()
}
