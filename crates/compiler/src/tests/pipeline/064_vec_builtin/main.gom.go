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
    var ret8 struct{}
    var v__0 []int32 = nil
    var v__1 []int32 = append(v__0, 10)
    var v__2 []int32 = append(v__1, 20)
    var v__3 []int32 = append(v__2, 30)
    var first__4 int32 = v__3[0]
    var second__5 int32 = v__3[1]
    var third__6 int32 = v__3[2]
    var len__7 int32 = int32(len(v__3))
    var t4 string = int32_to_string(first__4)
    string_println(t4)
    var t5 string = int32_to_string(second__5)
    string_println(t5)
    var t6 string = int32_to_string(third__6)
    string_println(t6)
    var t7 string = int32_to_string(len__7)
    string_println(t7)
    ret8 = struct{}{}
    return ret8
}

func main() {
    main0()
}
