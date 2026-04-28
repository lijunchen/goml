package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func _goml_Lib_x3a__x3a_msg() string {
    var retv1 string
    retv1 = "hi"
    return retv1
}

func main0() struct{} {
    var t3 string = _goml_Lib_x3a__x3a_msg()
    string_println(t3)
    return struct{}{}
}

func main() {
    main0()
}
