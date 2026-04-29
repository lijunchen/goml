package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_5int32 struct {
    _0 int32
    _1 int32
}

type GoError = error

func _goml_trait_x5f_impl_x23_ToString_x23__x28_int32_x2c_int32_x29__x23_to_x5f_string(self__0 Tuple2_5int32_5int32) string {
    var retv4 string
    retv4 = "(?, ?)"
    return retv4
}

func main0() struct{} {
    var x__1 int32 = 123
    var t6 string = int32_to_string(x__1)
    println__T_string(t6)
    var x__2 bool = true
    var t7 string = bool_to_string(x__2)
    println__T_string(t7)
    var x__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 3,
        _1: 4,
    }
    var t8 string = _goml_trait_x5f_impl_x23_ToString_x23__x28_int32_x2c_int32_x29__x23_to_x5f_string(x__3)
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
