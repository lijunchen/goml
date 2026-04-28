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

func int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func a_value() int8 {
    var retv12 int8
    retv12 = 90
    return retv12
}

func b_value() int8 {
    var retv14 int8
    var t15 int8 = -20
    retv14 = t15
    return retv14
}

func c_value() int8 {
    var retv17 int8
    retv17 = 3
    return retv17
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t19 string = int8_to_string(value__1)
    var t20 string = label__0 + t19
    string_println(t20)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t22 string = bool_to_string(value__3)
    var t23 string = label__2 + t22
    string_println(t23)
    return struct{}{}
}

func main0() struct{} {
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
    return struct{}{}
}

func main() {
    main0()
}
