package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_Lib_x3a__x3a_msg() string {
    var ret1 string
    ret1 = "hi"
    return ret1
}

func main0() struct{} {
    var ret2 struct{}
    var t0 string = _goml_Lib_x3a__x3a_msg()
    ret2 = string_println(t0)
    return ret2
}

func main() {
    main0()
}
