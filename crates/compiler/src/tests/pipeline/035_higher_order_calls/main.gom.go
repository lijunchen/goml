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

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int32) func(int32) int32 {
    return arr[index]
}

type GoError = error

func double(x__0 int32) int32 {
    var retv4 int32
    var t5 int32 = x__0 * 2
    retv4 = t5
    return retv4
}

func increment(x__1 int32) int32 {
    var retv7 int32
    var t8 int32 = x__1 + 1
    retv7 = t8
    return retv7
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv10 func(int32) int32
    var jp12 func(int32) int32
    if flag__2 {
        jp12 = double
    } else {
        jp12 = increment
    }
    retv10 = jp12
    return retv10
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t14 int32 = f__4(10)
    var t15 int32 = g__5(t14)
    println__T_int32(t15)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t16 func(int32) int32 = chooser(false)
    var direct__8 int32 = t16(5)
    var printer__9 func(string) struct{} = string_println
    var t17 string = int32_to_string(applied__7)
    printer__9(t17)
    var t18 string = int32_to_string(direct__8)
    printer__9(t18)
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
