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
    var t2 bool = x__0 < y__1
    var t3 bool = !t2
    var t4 bool = y__1 < x__0
    var t5 bool = !t4
    var t6 bool = t3 && t5
    retv1 = t6
    return retv1
}

func sum(n__2 int32) int32 {
    var retv8 int32
    var t11 bool = my_int_equal(n__2, 1)
    var jp10 int32
    if t11 {
        jp10 = 1
    } else {
        var t12 int32 = n__2 - 1
        var t13 int32 = sum(t12)
        var t14 int32 = n__2 + t13
        jp10 = t14
    }
    retv8 = jp10
    return retv8
}

func main0() struct{} {
    var t16 int32 = sum(100)
    println__T_int32(t16)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t19 string = int32_to_string(value__1)
    string_println(t19)
    return struct{}{}
}

func main() {
    main0()
}
