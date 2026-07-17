package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    var retv61 int32
    retv61 = 10
    return retv61
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv63 int32
    retv63 = 20
    return retv63
}

func main0() struct{} {
    var t65 S = S{}
    var t66 int32 = pick_a__T_S(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    var t68 S = S{}
    var t69 int32 = pick_b__T_S(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    println__T_string(t70)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv75 int32
    var t76 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv75 = t76
    return retv75
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__2)
    retv78 = t79
    return retv78
}

func pick_b__T_S(x__3 S) int32 {
    var retv81 int32
    var t82 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv84 string
    retv84 = self__34
    return retv84
}

func main() {
    main0()
}
