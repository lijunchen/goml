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

func array_get__Array_2_Fn_int32_to_int32(arr [2]func(int32) int32, index int32) func(int32) int32 {
    return arr[index]
}

func double(x__0 int32) int32 {
    var t3 int32 = x__0 * 2
    return t3
}

func increment(x__1 int32) int32 {
    var t4 int32 = x__1 + 1
    return t4
}

func chooser(flag__2 bool) func(int32) int32 {
    var jp6 func(int32) int32
    if flag__2 {
        jp6 = double
    } else {
        jp6 = increment
    }
    return jp6
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(xs__3, 1)
    var t7 int32 = f__4(10)
    var t8 int32 = g__5(t7)
    println__T_int32(t8)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t9 func(int32) int32 = chooser(false)
    var direct__8 int32 = t9(5)
    var printer__9 func(string) struct{} = string_println
    var t10 string = int32_to_string(applied__7)
    printer__9(t10)
    var t11 string = int32_to_string(direct__8)
    printer__9(t11)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t12 string = int32_to_string(value__1)
    var t13 struct{} = string_println(t12)
    return t13
}

func main() {
    main0()
}
