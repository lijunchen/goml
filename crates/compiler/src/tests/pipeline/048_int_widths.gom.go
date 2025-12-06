package main

import (
    "fmt"
)

func int16_to_string(x int16) string {
    return fmt.Sprintf("%d", x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func int64_to_string(x int64) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret13 struct{}
    var start16__0 int16 = 300
    var delta16__1 int16 = 45
    var sum16__2 int16 = start16__0 + delta16__1
    var flipped16__3 int16 = -start16__0
    var base32__4 int32 = 100000
    var more32__5 int32 = 200000
    var sum32__6 int32 = base32__4 + more32__5
    var diff32__7 int32 = sum32__6 - base32__4
    var big64__8 int64 = 5000000000
    var step64__9 int64 = 2000000000
    var remain64__10 int64 = big64__8 - step64__9
    var neg64__11 int64 = -step64__9
    var t8 string = int16_to_string(sum16__2)
    var t7 string = t8 + ", "
    var t9 string = int16_to_string(flipped16__3)
    var t6 string = t7 + t9
    var t5 string = t6 + "; "
    var t10 string = int32_to_string(diff32__7)
    var t4 string = t5 + t10
    var t3 string = t4 + "; "
    var t11 string = int64_to_string(remain64__10)
    var t2 string = t3 + t11
    var t1 string = t2 + "; "
    var t12 string = int64_to_string(neg64__11)
    var message__12 string = t1 + t12
    string_println(message__12)
    ret13 = struct{}{}
    return ret13
}

func main() {
    main0()
}
