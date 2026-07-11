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
    var retv7 int32
    retv7 = 10
    return retv7
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv9 int32
    retv9 = 20
    return retv9
}

func main0() struct{} {
    var t11 S = S{}
    var t12 int32 = pick_a__T_S(t11)
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t12)
    println__T_string(t13)
    var t14 S = S{}
    var t15 int32 = pick_b__T_S(t14)
    var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t15)
    println__T_string(t16)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t18 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t18)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv21 int32
    var t22 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv21 = t22
    return retv21
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv24 string
    var t25 string = _goml_runtime_core_int32_to_string(self__2)
    retv24 = t25
    return retv24
}

func pick_b__T_S(x__3 S) int32 {
    var retv27 int32
    var t28 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv30 string
    retv30 = self__9
    return retv30
}

func main() {
    main0()
}
