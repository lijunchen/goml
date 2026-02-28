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

func _goml_inherent_Point_Point_to_json(self__0 Point) string {
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t14 string = "{" + "\"x\":"
    var t15 string = int32_to_string(x__1)
    var t16 string = t14 + t15
    var t17 string = t16 + ","
    var t18 string = t17 + "\"y\":"
    var t19 string = int32_to_string(y__2)
    var t20 string = t18 + t19
    var t21 string = t20 + "}"
    return t21
}

func _goml_inherent_Person_Person_to_json(self__3 Person) string {
    var mtmp3 Person = self__3
    var x4 string = mtmp3.name
    var x5 int32 = mtmp3.age
    var x6 bool = mtmp3.active
    var active__6 bool = x6
    var age__5 int32 = x5
    var name__4 string = x4
    var t22 string = "{" + "\"name\":"
    var t23 string = json_escape_string(name__4)
    var t24 string = t22 + t23
    var t25 string = t24 + ","
    var t26 string = t25 + "\"age\":"
    var t27 string = int32_to_string(age__5)
    var t28 string = t26 + t27
    var t29 string = t28 + ","
    var t30 string = t29 + "\"active\":"
    var t31 string = bool_to_json(active__6)
    var t32 string = t30 + t31
    var t33 string = t32 + "}"
    return t33
}

func _goml_inherent_Color_Color_to_json(self__7 Color) string {
    var jp35 string
    switch self__7.(type) {
    case Red:
        jp35 = "{\"tag\":\"Red\"}"
    case Green:
        jp35 = "{\"tag\":\"Green\"}"
    case Blue:
        jp35 = "{\"tag\":\"Blue\"}"
    case Rgb:
        var x7 int32 = self__7.(Rgb)._0
        var x8 int32 = self__7.(Rgb)._1
        var x9 int32 = self__7.(Rgb)._2
        var __field2__10 int32 = x9
        var __field1__9 int32 = x8
        var __field0__8 int32 = x7
        var t36 string = int32_to_string(__field0__8)
        var t37 string = "{\"tag\":\"Rgb\",\"fields\":[" + t36
        var t38 string = t37 + ","
        var t39 string = int32_to_string(__field1__9)
        var t40 string = t38 + t39
        var t41 string = t40 + ","
        var t42 string = int32_to_string(__field2__10)
        var t43 string = t41 + t42
        var t44 string = t43 + "]}"
        jp35 = t44
    default:
        panic("non-exhaustive match")
    }
    return jp35
}

func main0() struct{} {
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
    var t45 string = _goml_inherent_Point_Point_to_json(p__11)
    string_println(t45)
    var t46 string = _goml_inherent_Person_Person_to_json(person__12)
    string_println(t46)
    var t47 string = _goml_inherent_Color_Color_to_json(c1__13)
    string_println(t47)
    var t48 string = _goml_inherent_Color_Color_to_json(c2__14)
    string_println(t48)
    return struct{}{}
}

func main() {
    main0()
}
