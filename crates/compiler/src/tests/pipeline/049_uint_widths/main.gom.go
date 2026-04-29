package main

import (
    _goml_fmt "fmt"
)

func uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var start8__0 uint8 = 200
    var add8__1 uint8 = 55
    var sum8__2 uint8 = start8__0 + add8__1
    var neg8__3 uint8 = -start8__0
    var start16__4 uint16 = 50000
    var add16__5 uint16 = 12000
    var sum16__6 uint16 = start16__4 + add16__5
    var diff16__7 uint16 = sum16__6 - start16__4
    var add32__9 uint32 = 123456789
    var neg32__11 uint32 = -add32__9
    var start64__12 uint64 = 6000000000
    var add64__13 uint64 = 4000000000
    var sum64__14 uint64 = start64__12 + add64__13
    var diff64__15 uint64 = sum64__14 - add64__13
    var t2 string = uint8_to_string(sum8__2)
    var t3 string = t2 + ", "
    var t4 string = uint8_to_string(neg8__3)
    var t5 string = t3 + t4
    var t6 string = t5 + "; "
    var t7 string = uint16_to_string(diff16__7)
    var t8 string = t6 + t7
    var t9 string = t8 + "; "
    var t10 string = uint32_to_string(neg32__11)
    var t11 string = t9 + t10
    var t12 string = t11 + "; "
    var t13 string = uint64_to_string(diff64__15)
    var message__16 string = t12 + t13
    println__T_string(message__16)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
