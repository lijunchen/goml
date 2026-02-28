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
    var t3 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t3 = x__0 * 2
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func increment(x__1 int32) int32 {
    var t4 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = x__1 + 1
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func chooser(flag__2 bool) func(int32) int32 {
    var jp6 func(int32) int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            if flag__2 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp6
        case 2:
            jp6 = double
            pc = 1
        case 3:
            jp6 = increment
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32
    var f__4 func(int32) int32
    var g__5 func(int32) int32
    var t7 int32
    var t8 int32
    var mtmp0 struct{}
    var chosen__6 func(int32) int32
    var applied__7 int32
    var t9 func(int32) int32
    var direct__8 int32
    var printer__9 func(string) struct{}
    var t10 string
    var mtmp1 struct{}
    var t11 string
    var mtmp2 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            xs__3 = [2]func(int32) int32{double, increment}
            f__4 = array_get__Array_2_Fn_int32_to_int32(xs__3, 0)
            g__5 = array_get__Array_2_Fn_int32_to_int32(xs__3, 1)
            t7 = f__4(10)
            t8 = g__5(t7)
            println__T_int32(t8)
            chosen__6 = chooser(true)
            applied__7 = chosen__6(5)
            t9 = chooser(false)
            direct__8 = t9(5)
            printer__9 = string_println
            t10 = int32_to_string(applied__7)
            printer__9(t10)
            t11 = int32_to_string(direct__8)
            printer__9(t11)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t12 string
    var t13 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t12 = int32_to_string(value__1)
            t13 = string_println(t12)
            return t13
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
