package main

import (
    _goml_fmt "fmt"
)

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

type Pair__uint8__float32 struct {
    first uint8
    second float32
}

type GoError = error

func main0() struct{} {
    var p__0 Pair__uint8__float32 = Pair__uint8__float32{
        first: 10,
        second: 3.140000104904175,
    }
    var t3 uint8 = p__0.first
    var t4 string = uint8_to_string(t3)
    println__T_string(t4)
    var t5 float32 = p__0.second
    var t6 string = float32_to_string(t5)
    println__T_string(t6)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
