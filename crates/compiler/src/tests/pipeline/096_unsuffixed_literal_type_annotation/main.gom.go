package main

import (
    "fmt"
)

func int8_to_string(x int8) string {
    return fmt.Sprintf("%d", x)
}

func int16_to_string(x int16) string {
    return fmt.Sprintf("%d", x)
}

func int64_to_string(x int64) string {
    return fmt.Sprintf("%d", x)
}

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

func float32_to_string(x float32) string {
    return fmt.Sprintf("%g", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func take_u8(x__0 uint8) uint8 {
    return x__0
}

func take_f32(x__1 float32) float32 {
    return x__1
}

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t10 string = uint8_to_string(a__2)
    string_println(t10)
    var t11 string = int8_to_string(b__3)
    string_println(t11)
    var t12 string = int16_to_string(c__4)
    string_println(t12)
    var t13 string = uint16_to_string(d__5)
    string_println(t13)
    var t14 string = uint32_to_string(e__6)
    string_println(t14)
    var t15 string = int64_to_string(f__7)
    string_println(t15)
    var t16 string = uint64_to_string(g__8)
    string_println(t16)
    var t17 string = float32_to_string(h__9)
    string_println(t17)
    var t18 uint8 = take_u8(10)
    var t19 string = uint8_to_string(t18)
    string_println(t19)
    var t20 float32 = take_f32(2.5)
    var t21 string = float32_to_string(t20)
    string_println(t21)
    return struct{}{}
}

func main() {
    main0()
}
