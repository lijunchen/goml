package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_Lib_p_msg() string {
    var retv1 string
    retv1 = "hi"
    return retv1
}

func main0() struct{} {
    var t3 string = _goml_m_Lib_p_msg()
    println__T_string(t3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
