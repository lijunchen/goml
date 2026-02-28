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
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var t14 string
    var t15 string
    var t16 string
    var t17 string
    var t18 string
    var t19 string
    var t20 string
    var t21 string
    mtmp0 = self__0
    x1 = mtmp0.x
    x2 = mtmp0.y
    y__2 = x2
    x__1 = x1
    t14 = "{" + "\"x\":"
    t15 = int32_to_string(x__1)
    t16 = t14 + t15
    t17 = t16 + ","
    t18 = t17 + "\"y\":"
    t19 = int32_to_string(y__2)
    t20 = t18 + t19
    t21 = t20 + "}"
    return t21
}

func _goml_inherent_Person_Person_to_json(self__3 Person) string {
    var mtmp3 Person
    var x4 string
    var x5 int32
    var x6 bool
    var active__6 bool
    var age__5 int32
    var name__4 string
    var t22 string
    var t23 string
    var t24 string
    var t25 string
    var t26 string
    var t27 string
    var t28 string
    var t29 string
    var t30 string
    var t31 string
    var t32 string
    var t33 string
    mtmp3 = self__3
    x4 = mtmp3.name
    x5 = mtmp3.age
    x6 = mtmp3.active
    active__6 = x6
    age__5 = x5
    name__4 = x4
    t22 = "{" + "\"name\":"
    t23 = json_escape_string(name__4)
    t24 = t22 + t23
    t25 = t24 + ","
    t26 = t25 + "\"age\":"
    t27 = int32_to_string(age__5)
    t28 = t26 + t27
    t29 = t28 + ","
    t30 = t29 + "\"active\":"
    t31 = bool_to_json(active__6)
    t32 = t30 + t31
    t33 = t32 + "}"
    return t33
}

func _goml_inherent_Color_Color_to_json(self__7 Color) string {
    var jp35 string
    var x7 int32
    var x8 int32
    var x9 int32
    var __field2__10 int32
    var __field1__9 int32
    var __field0__8 int32
    var t36 string
    var t37 string
    var t38 string
    var t39 string
    var t40 string
    var t41 string
    var t42 string
    var t43 string
    var t44 string
    switch self__7.(type) {
    case Red:
        goto b2
    case Green:
        goto b3
    case Blue:
        goto b4
    case Rgb:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp35
    b2:
    jp35 = "{\"tag\":\"Red\"}"
    goto b1
    b3:
    jp35 = "{\"tag\":\"Green\"}"
    goto b1
    b4:
    jp35 = "{\"tag\":\"Blue\"}"
    goto b1
    b5:
    x7 = self__7.(Rgb)._0
    x8 = self__7.(Rgb)._1
    x9 = self__7.(Rgb)._2
    __field2__10 = x9
    __field1__9 = x8
    __field0__8 = x7
    t36 = int32_to_string(__field0__8)
    t37 = "{\"tag\":\"Rgb\",\"fields\":[" + t36
    t38 = t37 + ","
    t39 = int32_to_string(__field1__9)
    t40 = t38 + t39
    t41 = t40 + ","
    t42 = int32_to_string(__field2__10)
    t43 = t41 + t42
    t44 = t43 + "]}"
    jp35 = t44
    goto b1
}

func main0() struct{} {
    var p__11 Point
    var person__12 Person
    var c1__13 Color
    var c2__14 Color
    var t45 string
    var t46 string
    var t47 string
    var t48 string
    p__11 = Point{
        x: 10,
        y: 20,
    }
    person__12 = Person{
        name: "Alice",
        age: 30,
        active: true,
    }
    c1__13 = Red{}
    c2__14 = Rgb{
        _0: 255,
        _1: 128,
        _2: 0,
    }
    t45 = _goml_inherent_Point_Point_to_json(p__11)
    string_println(t45)
    t46 = _goml_inherent_Person_Person_to_json(person__12)
    string_println(t46)
    t47 = _goml_inherent_Color_Color_to_json(c1__13)
    string_println(t47)
    t48 = _goml_inherent_Color_Color_to_json(c2__14)
    string_println(t48)
    return struct{}{}
}

func main() {
    main0()
}
