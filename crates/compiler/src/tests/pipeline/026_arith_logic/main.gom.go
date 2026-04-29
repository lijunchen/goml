package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func show_int(label__0 string, value__1 int32) struct{} {
    var t13 string = int32_to_string(value__1)
    var t14 string = label__0 + t13
    println__T_string(t14)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t16 string = bool_to_string(value__3)
    var t17 string = label__2 + t16
    println__T_string(t17)
    return struct{}{}
}

func main0() struct{} {
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    show_int("sum=", sum__5)
    show_int("diff=", diff__6)
    show_int("prod=", prod__7)
    show_int("quot=", quot__8)
    var and_result__9 bool = true && false
    var or_result__10 bool = true || false
    var not_result__11 bool = !false
    var t19 bool = !and_result__9
    var t20 int32 = prod__7 * base__4
    var t21 int32 = sum__5 + t20
    var t22 int32 = prod__7 / 2
    var mtmp6 int32 = t21 - t22
    var jp24 bool
    switch mtmp6 {
    case 0:
        jp24 = false
    default:
        jp24 = true
    }
    var t25 bool = t19 && jp24
    var t26 int32 = diff__6 - quot__8
    var t27 int32 = t26 + base__4
    var t28 int32 = sum__5 / 2
    var mtmp7 int32 = t27 - t28
    var jp30 bool
    switch mtmp7 {
    case 0:
        jp30 = false
    default:
        jp30 = true
    }
    var t31 bool = !jp30
    var mixed__12 bool = t25 || t31
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
