package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_Lib_x3a__x3a_msg() string {
    return "hi"
}

func main0() struct{} {
    var t0 string = _goml_Lib_x3a__x3a_msg()
    var t1 struct{} = string_println(t0)
    return t1
}

func main() {
    main0()
}
