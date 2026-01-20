package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {}

func goml__trait__impl_x23_A_x23_S_x23_foo(self__0 S) string {
    var ret4 string
    ret4 = "A"
    return ret4
}

func goml__trait__impl_x23_C_x23_S_x23_bar(self__2 S) string {
    var ret6 string
    ret6 = "C"
    return ret6
}

func main0() struct{} {
    var ret7 struct{}
    var s__5 S = S{}
    var t2 string = pick_a__T_S(s__5)
    string_println(t2)
    var t3 string = bar_it__T_S(s__5)
    string_println(t3)
    ret7 = struct{}{}
    return ret7
}

func pick_a__T_S(x__3 S) string {
    var ret8 string
    ret8 = goml__trait__impl_x23_A_x23_S_x23_foo(x__3)
    return ret8
}

func bar_it__T_S(x__4 S) string {
    var ret9 string
    ret9 = goml__trait__impl_x23_C_x23_S_x23_bar(x__4)
    return ret9
}

func main() {
    main0()
}
