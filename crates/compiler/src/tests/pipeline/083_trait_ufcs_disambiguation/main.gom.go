package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_trait_impl_A_S_pick(self__0 S) int32 {
    var ret8 int32
    ret8 = 10
    return ret8
}

func _goml_trait_impl_B_S_pick(self__1 S) int32 {
    var ret9 int32
    ret9 = 20
    return ret9
}

func main0() struct{} {
    var ret10 struct{}
    var t4 S = S{}
    var t3 int32 = pick_a__T_S(t4)
    var t2 string = int32_to_string(t3)
    println__T_string(t2)
    var t7 S = S{}
    var t6 int32 = pick_b__T_S(t7)
    var t5 string = int32_to_string(t6)
    println__T_string(t5)
    ret10 = struct{}{}
    return ret10
}

func pick_a__T_S(x__2 S) int32 {
    var ret11 int32
    ret11 = _goml_trait_impl_A_S_pick(x__2)
    return ret11
}

func println__T_string(value__1 string) struct{} {
    var ret12 struct{}
    ret12 = string_println(value__1)
    return ret12
}

func pick_b__T_S(x__3 S) int32 {
    var ret13 int32
    ret13 = _goml_trait_impl_B_S_pick(x__3)
    return ret13
}

func main() {
    main0()
}
