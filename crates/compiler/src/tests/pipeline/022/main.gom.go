package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func match_string(s__0 string) int32 {
    var retv6 int32
    var jp8 int32
    switch s__0 {
    case "hello":
        jp8 = 1
    case "world":
        jp8 = 2
    default:
        jp8 = 3
    }
    retv6 = jp8
    return retv6
}

func wildcard_position(s__1 string) int32 {
    var retv10 int32
    retv10 = 4
    return retv10
}

func repeated_string(s__2 string) int32 {
    var retv12 int32
    var jp14 int32
    switch s__2 {
    case "hello":
        jp14 = 6
    default:
        jp14 = 8
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var t16 int32 = match_string("hello")
    var t17 string = int32_to_string(t16)
    println__T_string(t17)
    var t18 int32 = match_string("planet")
    var t19 string = int32_to_string(t18)
    println__T_string(t19)
    var t20 int32 = wildcard_position("world")
    var t21 string = int32_to_string(t20)
    println__T_string(t21)
    var t22 int32 = wildcard_position("sun")
    var t23 string = int32_to_string(t22)
    println__T_string(t23)
    var t24 int32 = repeated_string("hello")
    var t25 string = int32_to_string(t24)
    println__T_string(t25)
    var t26 int32 = repeated_string("mars")
    var t27 string = int32_to_string(t26)
    println__T_string(t27)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
