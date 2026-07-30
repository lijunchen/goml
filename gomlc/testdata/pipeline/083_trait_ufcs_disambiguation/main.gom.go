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
    var retv111 int32
    retv111 = 10
    return retv111
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv113 int32
    retv113 = 20
    return retv113
}

func main0() struct{} {
    var t115 S = S{}
    var t116 int32 = pick_a__T_S(t115)
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t116)
    println__T_string(t117)
    var t118 S = S{}
    var t119 int32 = pick_b__T_S(t118)
    var t120 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t119)
    println__T_string(t120)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv125 int32
    var t126 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv128 string
    var t129 string = _goml_runtime_core_int32_to_string(self__6)
    retv128 = t129
    return retv128
}

func pick_b__T_S(x__3 S) int32 {
    var retv131 int32
    var t132 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv134 string
    retv134 = self__38
    return retv134
}

func main() {
    main0()
}
