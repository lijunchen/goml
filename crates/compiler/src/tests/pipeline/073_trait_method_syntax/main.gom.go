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

type S struct {
    value int32
}

type GoError = error

func _goml_trait_x5f_impl_x23_ToString_x23_S_x23_to_x5f_string(self__0 S) string {
    var retv2 string
    var t3 int32 = self__0.value
    var t4 string = int32_to_string(t3)
    var t5 string = "S(" + t4
    var t6 string = t5 + ")"
    retv2 = t6
    return retv2
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t8 string = _goml_trait_x5f_impl_x23_ToString_x23_S_x23_to_x5f_string(s__1)
    println__T_string(t8)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
