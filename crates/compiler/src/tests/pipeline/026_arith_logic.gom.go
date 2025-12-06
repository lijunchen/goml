package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func show_int(label__0 string, value__1 int32) struct{} {
    var ret27 struct{}
    var t13 string = int32_to_string(value__1)
    var t12 string = label__0 + t13
    string_println(t12)
    ret27 = struct{}{}
    return ret27
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var ret28 struct{}
    var t15 string = bool_to_string(value__3)
    var t14 string = label__2 + t15
    string_println(t14)
    ret28 = struct{}{}
    return ret28
}

func main0() struct{} {
    var ret29 struct{}
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
    var t17 bool = !and_result__9
    var t20 int32 = prod__7 * base__4
    var t19 int32 = sum__5 + t20
    var t21 int32 = prod__7 / 2
    var mtmp6 int32 = t19 - t21
    var t18 bool
    switch mtmp6 {
    case 0:
        t18 = false
    default:
        t18 = true
    }
    var t16 bool = t17 && t18
    var t25 int32 = diff__6 - quot__8
    var t24 int32 = t25 + base__4
    var t26 int32 = sum__5 / 2
    var mtmp7 int32 = t24 - t26
    var t23 bool
    switch mtmp7 {
    case 0:
        t23 = false
    default:
        t23 = true
    }
    var t22 bool = !t23
    var mixed__12 bool = t16 || t22
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    ret29 = struct{}{}
    return ret29
}

func main() {
    main0()
}
