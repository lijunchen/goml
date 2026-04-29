package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func _goml_dep_x3a__x3a_msg() string {
    var retv1 string
    retv1 = "hi"
    return retv1
}

func _goml_app_x3a__x3a_main() struct{} {
    var t3 string = _goml_dep_x3a__x3a_msg()
    println__T_string(t3)
    return struct{}{}
}

func main0() struct{} {
    _goml_app_x3a__x3a_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
