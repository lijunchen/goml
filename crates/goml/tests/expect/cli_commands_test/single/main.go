package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func _goml_single_x3a__x3a_main() struct{} {
    println__T_string("ok")
    return struct{}{}
}

func main0() struct{} {
    _goml_single_x3a__x3a_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
