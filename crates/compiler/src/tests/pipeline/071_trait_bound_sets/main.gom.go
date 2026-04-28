package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {}

type GoError = error

func _goml_trait_x5f_impl_x23_A_x23_S_x23_foo(self__0 S) string {
    var retv3 string
    retv3 = "A"
    return retv3
}

func _goml_trait_x5f_impl_x23_C_x23_S_x23_bar(self__2 S) string {
    var retv7 string
    retv7 = "C"
    return retv7
}

func main0() struct{} {
    var s__5 S = S{}
    var t9 string = pick_a__T_S(s__5)
    println__T_string(t9)
    var t10 string = bar_it__T_S(s__5)
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv14 string
    var t15 string = _goml_trait_x5f_impl_x23_A_x23_S_x23_foo(x__3)
    retv14 = t15
    return retv14
}

func bar_it__T_S(x__4 S) string {
    var retv17 string
    var t18 string = _goml_trait_x5f_impl_x23_C_x23_S_x23_bar(x__4)
    retv17 = t18
    return retv17
}

func main() {
    main0()
}
