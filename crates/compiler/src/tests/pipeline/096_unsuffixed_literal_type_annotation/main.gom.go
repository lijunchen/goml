package main

import (
    _goml_fmt "fmt"
)

func int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func take_u8(x__0 uint8) uint8 {
    var retv11 uint8
    retv11 = x__0
    return retv11
}

func take_f32(x__1 float32) float32 {
    var retv13 float32
    retv13 = x__1
    return retv13
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
    var t15 string = uint8_to_string(a__2)
    println__T_string(t15)
    var t16 string = int8_to_string(b__3)
    println__T_string(t16)
    var t17 string = int16_to_string(c__4)
    println__T_string(t17)
    var t18 string = uint16_to_string(d__5)
    println__T_string(t18)
    var t19 string = uint32_to_string(e__6)
    println__T_string(t19)
    var t20 string = int64_to_string(f__7)
    println__T_string(t20)
    var t21 string = uint64_to_string(g__8)
    println__T_string(t21)
    var t22 string = float32_to_string(h__9)
    println__T_string(t22)
    var t23 uint8 = take_u8(10)
    var t24 string = uint8_to_string(t23)
    println__T_string(t24)
    var t25 float32 = take_f32(2.5)
    var t26 string = float32_to_string(t25)
    println__T_string(t26)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
