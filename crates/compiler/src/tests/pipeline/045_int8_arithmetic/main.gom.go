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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 90
        default:
            panic("invalid pc")
        }
    }
}

func b_value() int8 {
    var t11 int8
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = -20
            return t11
        default:
            panic("invalid pc")
        }
    }
}

func c_value() int8 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 3
        default:
            panic("invalid pc")
        }
    }
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t12 string
    var t13 string
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t12 = int8_to_string(value__1)
            t13 = label__0 + t12
            string_println(t13)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t14 string
    var t15 string
    var mtmp1 struct{}
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t14 = bool_to_string(value__3)
            t15 = label__2 + t14
            string_println(t15)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var a__4 int8
    var b__5 int8
    var c__6 int8
    var sum__7 int8
    var diff__8 int8
    var prod__9 int8
    var quot__10 int8
    var neg__11 int8
    var less__12 bool
    var mtmp2 struct{}
    var mtmp3 struct{}
    var mtmp4 struct{}
    var mtmp5 struct{}
    var mtmp6 struct{}
    var mtmp7 struct{}
    var mtmp8 struct{}
    var mtmp9 struct{}
    var mtmp10 struct{}
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    _ = mtmp6
    _ = mtmp7
    _ = mtmp8
    _ = mtmp9
    _ = mtmp10
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__4 = a_value()
            b__5 = b_value()
            c__6 = c_value()
            sum__7 = a__4 + b__5
            diff__8 = a__4 - c__6
            prod__9 = b__5 * c__6
            quot__10 = a__4 / c__6
            neg__11 = -b__5
            less__12 = b__5 < a__4
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
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
