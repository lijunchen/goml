package main

import (
    "fmt"
)

func uint8_to_string(x uint8) string {
    return fmt.Sprintf("%d", x)
}

func float32_to_string(x float32) string {
    return fmt.Sprintf("%g", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Pair__uint8__float32 struct {
    first uint8
    second float32
}

func main0() struct{} {
    var p__0 Pair__uint8__float32 = Pair__uint8__float32{
        first: 10,
        second: 3.140000104904175,
    }
    var t2 uint8 = p__0.first
    var t3 string = uint8_to_string(t2)
    string_println(t3)
    var t4 float32 = p__0.second
    var t5 string = float32_to_string(t4)
    string_println(t5)
    return struct{}{}
}

func main() {
    main0()
}
