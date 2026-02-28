package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func my_int_equal(x__0 int32, y__1 int32) bool {
    var t0 bool = x__0 < y__1
    var t1 bool = !t0
    var t2 bool = y__1 < x__0
    var t3 bool = !t2
    var t4 bool = t1 && t3
    return t4
}

func sum(n__2 int32) int32 {
    var t7 bool = my_int_equal(n__2, 1)
    var jp6 int32
    if t7 {
        jp6 = 1
    } else {
        var t8 int32 = n__2 - 1
        var t9 int32 = sum(t8)
        var t10 int32 = n__2 + t9
        jp6 = t10
    }
    return jp6
}

func main0() struct{} {
    var t11 int32 = sum(100)
    var t12 struct{} = println__T_int32(t11)
    return t12
}

func println__T_int32(value__1 int32) struct{} {
    var t13 string = int32_to_string(value__1)
    var t14 struct{} = string_println(t13)
    return t14
}

func main() {
    main0()
}
