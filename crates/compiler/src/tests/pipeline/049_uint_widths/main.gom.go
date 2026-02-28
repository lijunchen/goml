package main

import (
    "fmt"
)

func uint8_to_string(x uint8) string {
    return fmt.Sprintf("%d", x)
}

func uint16_to_string(x uint16) string {
    return fmt.Sprintf("%d", x)
}

func uint32_to_string(x uint32) string {
    return fmt.Sprintf("%d", x)
}

func uint64_to_string(x uint64) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var start8__0 uint8
    var add8__1 uint8
    var sum8__2 uint8
    var neg8__3 uint8
    var start16__4 uint16
    var add16__5 uint16
    var sum16__6 uint16
    var diff16__7 uint16
    var start32__8 uint32
    var add32__9 uint32
    var sum32__10 uint32
    var neg32__11 uint32
    var start64__12 uint64
    var add64__13 uint64
    var sum64__14 uint64
    var diff64__15 uint64
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
    var message__16 string
    var _wild0 struct{}
    _ = sum32__10
    _ = _wild0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            start8__0 = 200
            add8__1 = 55
            sum8__2 = start8__0 + add8__1
            neg8__3 = -start8__0
            start16__4 = 50000
            add16__5 = 12000
            sum16__6 = start16__4 + add16__5
            diff16__7 = sum16__6 - start16__4
            start32__8 = 3000000000
            add32__9 = 123456789
            neg32__11 = -add32__9
            start64__12 = 6000000000
            add64__13 = 4000000000
            sum64__14 = start64__12 + add64__13
            diff64__15 = sum64__14 - add64__13
            t1 = uint8_to_string(sum8__2)
            t2 = t1 + ", "
            t3 = uint8_to_string(neg8__3)
            t4 = t2 + t3
            t5 = t4 + "; "
            t6 = uint16_to_string(diff16__7)
            t7 = t5 + t6
            t8 = t7 + "; "
            t9 = uint32_to_string(neg32__11)
            t10 = t8 + t9
            t11 = t10 + "; "
            t12 = uint64_to_string(diff64__15)
            message__16 = t11 + t12
            string_println(message__16)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
