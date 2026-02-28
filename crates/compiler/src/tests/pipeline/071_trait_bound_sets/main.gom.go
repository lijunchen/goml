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
    return "A"
}

func _goml_trait_impl_C_S_bar(self__2 S) string {
    return "C"
}

func main0() struct{} {
    var s__5 S = S{}
    var t2 string = pick_a__T_S(s__5)
    println__T_string(t2)
    var t3 string = bar_it__T_S(s__5)
    println__T_string(t3)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var t4 string = _goml_trait_impl_A_S_foo(x__3)
    return t4
}

func println__T_string(value__1 string) struct{} {
    var t5 struct{} = string_println(value__1)
    return t5
}

func bar_it__T_S(x__4 S) string {
    var t6 string = _goml_trait_impl_C_S_bar(x__4)
    return t6
}

func main() {
    main0()
}
