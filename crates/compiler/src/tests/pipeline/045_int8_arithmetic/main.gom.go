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
    var ret517 int8
    ret517 = 90
    return ret517
}

func b_value() int8 {
    var ret518 int8
    ret518 = -20
    return ret518
}

func c_value() int8 {
    var ret519 int8
    ret519 = 3
    return ret519
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var ret520 struct{}
    var t514 string = int8_to_string(value__1)
    var t513 string = label__0 + t514
    string_println(t513)
    ret520 = struct{}{}
    return ret520
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var ret521 struct{}
    var t516 string = bool_to_string(value__3)
    var t515 string = label__2 + t516
    string_println(t515)
    ret521 = struct{}{}
    return ret521
}

func main0() struct{} {
    var ret522 struct{}
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
    ret522 = struct{}{}
    return ret522
}

func main() {
    main0()
}
