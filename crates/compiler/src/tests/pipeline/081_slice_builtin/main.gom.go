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

func main0() struct{} {
    var ret15 struct{}
    var v__0 []int32 = nil
    var v__1 []int32 = append(v__0, 10)
    var v__2 []int32 = append(v__1, 20)
    var v__3 []int32 = append(v__2, 30)
    var v__4 []int32 = append(v__3, 40)
    var s__5 []int32 = v__4[1:4]
    var t7 int32 = int32(len(s__5))
    println__T_int32(t7)
    var t8 int32 = s__5[0]
    println__T_int32(t8)
    var t9 int32 = s__5[1]
    println__T_int32(t9)
    var t10 int32 = s__5[2]
    println__T_int32(t10)
    var t__6 []int32 = s__5[1:3]
    var t11 int32 = int32(len(t__6))
    println__T_int32(t11)
    var t12 int32 = t__6[0]
    println__T_int32(t12)
    var t13 int32 = t__6[1]
    println__T_int32(t13)
    ret15 = struct{}{}
    return ret15
}

func println__T_int32(value__1 int32) struct{} {
    var ret16 struct{}
    var t14 string = int32_to_string(value__1)
    ret16 = string_println(t14)
    return ret16
}

func main() {
    main0()
}
