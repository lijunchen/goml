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
    var t12 string
    var t13 string
    t12 = int32_to_string(value__1)
    t13 = label__0 + t12
    string_println(t13)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t14 string
    var t15 string
    t14 = bool_to_string(value__3)
    t15 = label__2 + t14
    string_println(t15)
    return struct{}{}
}

func main0() struct{} {
    var base__4 int32
    var sum__5 int32
    var diff__6 int32
    var prod__7 int32
    var quot__8 int32
    var and_result__9 bool
    var or_result__10 bool
    var not_result__11 bool
    var t16 bool
    var t17 int32
    var t18 int32
    var t19 int32
    var mtmp6 int32
    var jp21 bool
    var t22 bool
    var t23 int32
    var t24 int32
    var t25 int32
    var mtmp7 int32
    var jp27 bool
    var t28 bool
    var mixed__12 bool
    base__4 = 10
    sum__5 = base__4 + 5
    diff__6 = sum__5 - 3
    prod__7 = diff__6 * 2
    quot__8 = prod__7 / 4
    show_int("sum=", sum__5)
    show_int("diff=", diff__6)
    show_int("prod=", prod__7)
    show_int("quot=", quot__8)
    and_result__9 = true && false
    or_result__10 = true || false
    not_result__11 = !false
    t16 = !and_result__9
    t17 = prod__7 * base__4
    t18 = sum__5 + t17
    t19 = prod__7 / 2
    mtmp6 = t18 - t19
    switch mtmp6 {
    case 0:
        goto b5
    default:
        goto b6
    }
    b1:
    t22 = t16 && jp21
    t23 = diff__6 - quot__8
    t24 = t23 + base__4
    t25 = sum__5 / 2
    mtmp7 = t24 - t25
    switch mtmp7 {
    case 0:
        goto b3
    default:
        goto b4
    }
    b2:
    t28 = !jp27
    mixed__12 = t22 || t28
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
    b3:
    jp27 = false
    goto b2
    b4:
    jp27 = true
    goto b2
    b5:
    jp21 = false
    goto b1
    b6:
    jp21 = true
    goto b1
}

func main() {
    main0()
}
