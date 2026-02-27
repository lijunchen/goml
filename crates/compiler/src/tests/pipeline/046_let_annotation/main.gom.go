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
    var ret6 struct{}
    var x__0 int32 = 1
    var y__1 int8 = 1
    print__T_string("int32: ")
    println__T_int32(x__0)
    print__T_string("int8: ")
    println__T_int8(y__1)
    ret6 = struct{}{}
    return ret6
}

func print__T_string(value__0 string) struct{} {
    var ret7 struct{}
    ret7 = string_print(value__0)
    return ret7
}

func println__T_int32(value__1 int32) struct{} {
    var ret8 struct{}
    var t4 string = int32_to_string(value__1)
    ret8 = string_println(t4)
    return ret8
}

func println__T_int8(value__1 int8) struct{} {
    var ret9 struct{}
    var t5 string = int8_to_string(value__1)
    ret9 = string_println(t5)
    return ret9
}

func main() {
    main0()
}
