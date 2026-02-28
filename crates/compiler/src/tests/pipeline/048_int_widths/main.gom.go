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
    var start16__0 int16
    var delta16__1 int16
    var sum16__2 int16
    var flipped16__3 int16
    var base32__4 int32
    var more32__5 int32
    var sum32__6 int32
    var diff32__7 int32
    var big64__8 int64
    var step64__9 int64
    var remain64__10 int64
    var neg64__11 int64
    var t1 string
    var t2 string
    var t3 string
    var t4 string
    var t5 string
    var t6 string
    var t7 string
    var t8 string
    var t9 string
    var t10 string
    var t11 string
    var t12 string
    var message__12 string
    var _wild0 struct{}
    _ = _wild0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            start16__0 = 300
            delta16__1 = 45
            sum16__2 = start16__0 + delta16__1
            flipped16__3 = -start16__0
            base32__4 = 100000
            more32__5 = 200000
            sum32__6 = base32__4 + more32__5
            diff32__7 = sum32__6 - base32__4
            big64__8 = 5000000000
            step64__9 = 2000000000
            remain64__10 = big64__8 - step64__9
            neg64__11 = -step64__9
            t1 = int16_to_string(sum16__2)
            t2 = t1 + ", "
            t3 = int16_to_string(flipped16__3)
            t4 = t2 + t3
            t5 = t4 + "; "
            t6 = int32_to_string(diff32__7)
            t7 = t5 + t6
            t8 = t7 + "; "
            t9 = int64_to_string(remain64__10)
            t10 = t8 + t9
            t11 = t10 + "; "
            t12 = int64_to_string(neg64__11)
            message__12 = t11 + t12
            string_println(message__12)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
