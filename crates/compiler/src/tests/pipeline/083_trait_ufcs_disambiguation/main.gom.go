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
    var retv67 int32
    retv67 = 10
    return retv67
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv69 int32
    retv69 = 20
    return retv69
}

func main0() struct{} {
    var t71 S = S{}
    var t72 int32 = pick_a__T_S(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    var t74 S = S{}
    var t75 int32 = pick_b__T_S(t74)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t75)
    println__T_string(t76)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv81 int32
    var t82 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__6)
    retv84 = t85
    return retv84
}

func pick_b__T_S(x__3 S) int32 {
    var retv87 int32
    var t88 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv90 string
    retv90 = self__38
    return retv90
}

func main() {
    main0()
}
