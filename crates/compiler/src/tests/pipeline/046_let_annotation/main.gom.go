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
    var ret17 struct{}
    var x__0 int32 = 1
    var y__1 int8 = 1
    print__T_string("int32: ")
    println__T_int32(x__0)
    print__T_string("int8: ")
    println__T_int8(y__1)
    ret17 = struct{}{}
    return ret17
}

func print__T_string(value__0 string) struct{} {
    var ret18 struct{}
    ret18 = string_print(value__0)
    return ret18
}

func println__T_int32(value__1 int32) struct{} {
    var ret19 struct{}
    var t15 string = int32_to_string(value__1)
    ret19 = string_println(t15)
    return ret19
}

func println__T_int8(value__1 int8) struct{} {
    var ret20 struct{}
    var t16 string = int8_to_string(value__1)
    ret20 = string_println(t16)
    return ret20
}

func main() {
    main0()
}
