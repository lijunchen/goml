package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_trait_impl_A_S_foo(self__0 S) string {
    var ret5 string
    ret5 = "A"
    return ret5
}

func _goml_trait_impl_C_S_bar(self__2 S) string {
    var ret7 string
    ret7 = "C"
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    var s__5 S = S{}
    var t3 string = pick_a__T_S(s__5)
    println__T_string(t3)
    var t4 string = bar_it__T_S(s__5)
    println__T_string(t4)
    ret8 = struct{}{}
    return ret8
}

func pick_a__T_S(x__3 S) string {
    var ret9 string
    ret9 = _goml_trait_impl_A_S_foo(x__3)
    return ret9
}

func println__T_string(value__1 string) struct{} {
    var ret10 struct{}
    ret10 = string_println(value__1)
    return ret10
}

func bar_it__T_S(x__4 S) string {
    var ret11 string
    ret11 = _goml_trait_impl_C_S_bar(x__4)
    return ret11
}

func main() {
    main0()
}
