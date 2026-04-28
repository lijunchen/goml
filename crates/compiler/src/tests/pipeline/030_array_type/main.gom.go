package main

import (
    _goml_fmt "fmt"
)

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

type GoError = error

func main0() struct{} {
    print__T_string("array")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func main() {
    main0()
}
