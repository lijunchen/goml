package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func my_int_equal(x__0 int32, y__1 int32) bool {
    var retv1 bool
    var t4 bool = x__0 < y__1
    var t5 bool = !t4
    var jp3 bool
    if t5 {
        var t6 bool = y__1 < x__0
        var t7 bool = !t6
        jp3 = t7
    } else {
        jp3 = false
    }
    retv1 = jp3
    return retv1
}

func sum(n__2 int32) int32 {
    var retv9 int32
    var t12 bool = my_int_equal(n__2, 1)
    var jp11 int32
    if t12 {
        jp11 = 1
    } else {
        var t13 int32 = n__2 - 1
        var t14 int32 = sum(t13)
        var t15 int32 = n__2 + t14
        jp11 = t15
    }
    retv9 = jp11
    return retv9
}

func main0() struct{} {
    var t17 int32 = sum(100)
    println__T_int32(t17)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t20 string = int32_to_string(value__1)
    string_println(t20)
    return struct{}{}
}

func main() {
    main0()
}
