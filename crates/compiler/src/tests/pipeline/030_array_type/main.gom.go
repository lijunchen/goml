package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

func main0() struct{} {
    var ret1 struct{}
    ret1 = print__T_string("array")
    return ret1
}

func print__T_string(value__0 string) struct{} {
    var ret2 struct{}
    ret2 = string_print(value__0)
    return ret2
}

func main() {
    main0()
}
