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

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__0 int8 = -128
    var t3 string = int8_to_string(a__0)
    string_println(t3)
    var b__1 int16 = -32768
    var t4 string = int16_to_string(b__1)
    string_println(t4)
    return struct{}{}
}

func main() {
    main0()
}
