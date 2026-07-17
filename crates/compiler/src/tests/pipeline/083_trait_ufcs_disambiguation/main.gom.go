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
    var retv64 int32
    retv64 = 10
    return retv64
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv66 int32
    retv66 = 20
    return retv66
}

func main0() struct{} {
    var t68 S = S{}
    var t69 int32 = pick_a__T_S(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    println__T_string(t70)
    var t71 S = S{}
    var t72 int32 = pick_b__T_S(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv78 int32
    var t79 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__5)
    retv81 = t82
    return retv81
}

func pick_b__T_S(x__3 S) int32 {
    var retv84 int32
    var t85 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv87 string
    retv87 = self__37
    return retv87
}

func main() {
    main0()
}
