package main

import (
    _goml_fmt "fmt"
)

func bool_to_json(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func json_escape_string(s string) string {
    return _goml_fmt.Sprintf("%q", s)
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

type GoError = error

func _goml_inherent_Point_Point_to_json(self__0 Point) string {
    var retv15 string
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t16 string = "{" + "\"x\":"
    var t17 string = int32_to_string(x__1)
    var t18 string = t16 + t17
    var t19 string = t18 + ","
    var t20 string = t19 + "\"y\":"
    var t21 string = int32_to_string(y__2)
    var t22 string = t20 + t21
    var t23 string = t22 + "}"
    retv15 = t23
    return retv15
}

func _goml_inherent_Person_Person_to_json(self__3 Person) string {
    var retv25 string
    var mtmp3 Person = self__3
    var x4 string = mtmp3.name
    var x5 int32 = mtmp3.age
    var x6 bool = mtmp3.active
    var active__6 bool = x6
    var age__5 int32 = x5
    var name__4 string = x4
    var t26 string = "{" + "\"name\":"
    var t27 string = json_escape_string(name__4)
    var t28 string = t26 + t27
    var t29 string = t28 + ","
    var t30 string = t29 + "\"age\":"
    var t31 string = int32_to_string(age__5)
    var t32 string = t30 + t31
    var t33 string = t32 + ","
    var t34 string = t33 + "\"active\":"
    var t35 string = bool_to_json(active__6)
    var t36 string = t34 + t35
    var t37 string = t36 + "}"
    retv25 = t37
    return retv25
}

func _goml_inherent_Color_Color_to_json(self__7 Color) string {
    var retv39 string
    var jp41 string
    switch self__7.(type) {
    case Red:
        jp41 = "{\"tag\":\"Red\"}"
    case Green:
        jp41 = "{\"tag\":\"Green\"}"
    case Blue:
        jp41 = "{\"tag\":\"Blue\"}"
    case Rgb:
        var x7 int32 = self__7.(Rgb)._0
        var x8 int32 = self__7.(Rgb)._1
        var x9 int32 = self__7.(Rgb)._2
        var __field2__10 int32 = x9
        var __field1__9 int32 = x8
        var __field0__8 int32 = x7
        var t42 string = int32_to_string(__field0__8)
        var t43 string = "{\"tag\":\"Rgb\",\"fields\":[" + t42
        var t44 string = t43 + ","
        var t45 string = int32_to_string(__field1__9)
        var t46 string = t44 + t45
        var t47 string = t46 + ","
        var t48 string = int32_to_string(__field2__10)
        var t49 string = t47 + t48
        var t50 string = t49 + "]}"
        jp41 = t50
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
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
    var t52 string = _goml_inherent_Point_Point_to_json(p__11)
    string_println(t52)
    var t53 string = _goml_inherent_Person_Person_to_json(person__12)
    string_println(t53)
    var t54 string = _goml_inherent_Color_Color_to_json(c1__13)
    string_println(t54)
    var t55 string = _goml_inherent_Color_Color_to_json(c2__14)
    string_println(t55)
    return struct{}{}
}

func main() {
    main0()
}
