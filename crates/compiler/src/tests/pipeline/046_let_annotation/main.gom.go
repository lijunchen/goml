package main

import (
    "fmt"
)

func int8_to_string(x int8) string {
    return fmt.Sprintf("%d", x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int32 = 1
    var y__1 int8 = 1
    print__T_string("int32: ")
    println__T_int32(x__0)
    print__T_string("int8: ")
    println__T_int8(y__1)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t4 struct{} = string_print(value__0)
    return t4
}

func println__T_int32(value__1 int32) struct{} {
    var t5 string = int32_to_string(value__1)
    var t6 struct{} = string_println(t5)
    return t6
}

func println__T_int8(value__1 int8) struct{} {
    var t7 string = int8_to_string(value__1)
    var t8 struct{} = string_println(t7)
    return t8
}

func main() {
    main0()
}
