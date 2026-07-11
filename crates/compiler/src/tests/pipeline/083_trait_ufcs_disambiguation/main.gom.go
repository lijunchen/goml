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
    var retv25 int32
    retv25 = 10
    return retv25
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv27 int32
    retv27 = 20
    return retv27
}

func main0() struct{} {
    var t29 S = S{}
    var t30 int32 = pick_a__T_S(t29)
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t30)
    println__T_string(t31)
    var t32 S = S{}
    var t33 int32 = pick_b__T_S(t32)
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t33)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv39 int32
    var t40 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int32_to_string(self__2)
    retv42 = t43
    return retv42
}

func pick_b__T_S(x__3 S) int32 {
    var retv45 int32
    var t46 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv45 = t46
    return retv45
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
