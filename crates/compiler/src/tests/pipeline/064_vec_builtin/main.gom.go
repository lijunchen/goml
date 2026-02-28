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
    var v__0 []int32
    var v__1 []int32
    var v__2 []int32
    var v__3 []int32
    var first__4 int32
    var second__5 int32
    var third__6 int32
    var len__7 int32
    var t4 string
    var mtmp0 struct{}
    var t5 string
    var mtmp1 struct{}
    var t6 string
    var mtmp2 struct{}
    var t7 string
    var mtmp3 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            v__0 = nil
            v__1 = append(v__0, 10)
            v__2 = append(v__1, 20)
            v__3 = append(v__2, 30)
            first__4 = v__3[0]
            second__5 = v__3[1]
            third__6 = v__3[2]
            len__7 = int32(len(v__3))
            t4 = int32_to_string(first__4)
            string_println(t4)
            t5 = int32_to_string(second__5)
            string_println(t5)
            t6 = int32_to_string(third__6)
            string_println(t6)
            t7 = int32_to_string(len__7)
            string_println(t7)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
