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

type GoError = error

func main0() struct{} {
    var v__0 []int32 = nil
    var v__1 []int32 = append(v__0, 10)
    var v__2 []int32 = append(v__1, 20)
    var v__3 []int32 = append(v__2, 30)
    var first__4 int32 = v__3[0]
    var second__5 int32 = v__3[1]
    var third__6 int32 = v__3[2]
    var len__7 int32 = int32(len(v__3))
    var t5 string = int32_to_string(first__4)
    println__T_string(t5)
    var t6 string = int32_to_string(second__5)
    println__T_string(t6)
    var t7 string = int32_to_string(third__6)
    println__T_string(t7)
    var t8 string = int32_to_string(len__7)
    println__T_string(t8)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
