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
    var t12 string = int32_to_string(value__1)
    var t13 string = label__0 + t12
    string_println(t13)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t14 string = bool_to_string(value__3)
    var t15 string = label__2 + t14
    string_println(t15)
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
    var t16 bool = !and_result__9
    var t17 int32 = prod__7 * base__4
    var t18 int32 = sum__5 + t17
    var t19 int32 = prod__7 / 2
    var mtmp6 int32 = t18 - t19
    var jp21 bool
    switch mtmp6 {
    case 0:
        jp21 = false
    default:
        jp21 = true
    }
    var t22 bool = t16 && jp21
    var t23 int32 = diff__6 - quot__8
    var t24 int32 = t23 + base__4
    var t25 int32 = sum__5 / 2
    var mtmp7 int32 = t24 - t25
    var jp27 bool
    switch mtmp7 {
    case 0:
        jp27 = false
    default:
        jp27 = true
    }
    var t28 bool = !jp27
    var mixed__12 bool = t22 || t28
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func main() {
    main0()
}
