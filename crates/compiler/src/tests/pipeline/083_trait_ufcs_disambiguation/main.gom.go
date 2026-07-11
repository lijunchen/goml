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
    var retv10 int32
    retv10 = 10
    return retv10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv12 int32
    retv12 = 20
    return retv12
}

func main0() struct{} {
    var t14 S = S{}
    var t15 int32 = pick_a__T_S(t14)
    var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t15)
    println__T_string(t16)
    var t17 S = S{}
    var t18 int32 = pick_b__T_S(t17)
    var t19 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t18)
    println__T_string(t19)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t21)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv24 int32
    var t25 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv24 = t25
    return retv24
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv27 string
    var t28 string = _goml_runtime_core_int32_to_string(self__2)
    retv27 = t28
    return retv27
}

func pick_b__T_S(x__3 S) int32 {
    var retv30 int32
    var t31 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv30 = t31
    return retv30
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv33 string
    retv33 = self__9
    return retv33
}

func main() {
    main0()
}
