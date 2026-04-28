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

type S struct {}

type GoError = error

func _goml_trait_impl_A_S_pick(self__0 S) int32 {
    var retv3 int32
    retv3 = 10
    return retv3
}

func _goml_trait_impl_B_S_pick(self__1 S) int32 {
    var retv5 int32
    retv5 = 20
    return retv5
}

func main0() struct{} {
    var t7 S = S{}
    var t8 int32 = pick_a__T_S(t7)
    var t9 string = int32_to_string(t8)
    println__T_string(t9)
    var t10 S = S{}
    var t11 int32 = pick_b__T_S(t10)
    var t12 string = int32_to_string(t11)
    println__T_string(t12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv16 int32
    var t17 int32 = _goml_trait_impl_A_S_pick(x__2)
    retv16 = t17
    return retv16
}

func pick_b__T_S(x__3 S) int32 {
    var retv19 int32
    var t20 int32 = _goml_trait_impl_B_S_pick(x__3)
    retv19 = t20
    return retv19
}

func main() {
    main0()
}
