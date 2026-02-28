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
    return 10
}

func _goml_trait_impl_B_S_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t2 S
    var t3 int32
    var t4 string
    var t5 S
    var t6 int32
    var t7 string
    t2 = S{}
    t3 = pick_a__T_S(t2)
    t4 = int32_to_string(t3)
    println__T_string(t4)
    t5 = S{}
    t6 = pick_b__T_S(t5)
    t7 = int32_to_string(t6)
    println__T_string(t7)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var t8 int32
    t8 = _goml_trait_impl_A_S_pick(x__2)
    return t8
}

func println__T_string(value__1 string) struct{} {
    var t9 struct{}
    t9 = string_println(value__1)
    return t9
}

func pick_b__T_S(x__3 S) int32 {
    var t10 int32
    t10 = _goml_trait_impl_B_S_pick(x__3)
    return t10
}

func main() {
    main0()
}
