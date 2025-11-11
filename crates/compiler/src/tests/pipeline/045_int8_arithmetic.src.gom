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

func int8_to_string(x int8) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func a_value() int8 {
    var ret15 int8
    ret15 = 90
    return ret15
}

func b_value() int8 {
    var ret16 int8
    ret16 = -20
    return ret16
}

func c_value() int8 {
    var ret17 int8
    ret17 = 3
    return ret17
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var ret18 struct{}
    var t12 string = int8_to_string(value__1)
    var t11 string = label__0 + t12
    string_println(t11)
    ret18 = struct{}{}
    return ret18
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var ret19 struct{}
    var t14 string = bool_to_string(value__3)
    var t13 string = label__2 + t14
    string_println(t13)
    ret19 = struct{}{}
    return ret19
}

func main0() struct{} {
    var ret20 struct{}
    var a__4 int8 = a_value()
    var b__5 int8 = b_value()
    var c__6 int8 = c_value()
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    show_int8("a=", a__4)
    show_int8("b=", b__5)
    show_int8("c=", c__6)
    show_int8("sum=", sum__7)
    show_int8("diff=", diff__8)
    show_int8("prod=", prod__9)
    show_int8("quot=", quot__10)
    show_int8("neg=", neg__11)
    show_bool("b<a=", less__12)
    ret20 = struct{}{}
    return ret20
}

func main() {
    main0()
}
