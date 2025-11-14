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
    var ret9 bool
    var t1 bool = x__0 < y__1
    var t0 bool = !t1
    var t3 bool = y__1 < x__0
    var t2 bool = !t3
    ret9 = t0 && t2
    return ret9
}

func sum(n__2 int32) int32 {
    var ret10 int32
    var t4 bool = my_int_equal(n__2, 1)
    if t4 {
        ret10 = 1
    } else {
        var t6 int32 = n__2 - 1
        var t5 int32 = sum(t6)
        ret10 = n__2 + t5
    }
    return ret10
}

func main0() struct{} {
    var ret11 struct{}
    var t8 int32 = sum(100)
    var t7 string = int32_to_string(t8)
    ret11 = string_println(t7)
    return ret11
}

func main() {
    main0()
}
