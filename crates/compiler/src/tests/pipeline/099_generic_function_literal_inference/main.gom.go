package main

import (
    _goml_fmt "fmt"
)

func int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func uint8_to_string(x uint8) string {
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

func main0() struct{} {
    var a__1 uint8 = identity__T_uint8(42)
    var t4 string = uint8_to_string(a__1)
    string_println(t4)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t5 string = float32_to_string(b__2)
    string_println(t5)
    var c__3 int64 = identity__T_int64(100)
    var t6 string = int64_to_string(c__3)
    string_println(t6)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv8 uint8
    retv8 = x__0
    return retv8
}

func identity__T_float32(x__0 float32) float32 {
    var retv10 float32
    retv10 = x__0
    return retv10
}

func identity__T_int64(x__0 int64) int64 {
    var retv12 int64
    retv12 = x__0
    return retv12
}

func main() {
    main0()
}
