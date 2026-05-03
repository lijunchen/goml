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

func loop_return_unit(flag__0 bool) struct{} {
    for {
        if flag__0 {
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
    return struct{}{}
}

func loop_return() int32 {
    var retv13 int32
    retv13 = 5
    return retv13
}

func main0() struct{} {
    loop_return_unit(true)
    var t17 int32 = loop_return()
    println__T_int32(t17)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t21 string = int32_to_string(value__1)
    string_println(t21)
    return struct{}{}
}

func main() {
    main0()
}
