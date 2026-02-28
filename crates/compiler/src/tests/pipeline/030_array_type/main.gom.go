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
    var t0 struct{} = print__T_string("array")
    return t0
}

func print__T_string(value__0 string) struct{} {
    var t1 struct{} = string_print(value__0)
    return t1
}

func main() {
    main0()
}
