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

func _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(self__0 []int32) int32 {
    var retv1 int32
    var t2 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv1 = t2
    return retv1
}

func main0() struct{} {
    var values__2 []int32 = nil
    var t4 int32 = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t5 string = int32_to_string(t4)
    println__T_string(t5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__73 []int32) int32 {
    var retv8 int32
    var t9 int32 = int32(len(self__73))
    retv8 = t9
    return retv8
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 []int32) int32 {
    var retv13 int32
    var t14 int32 = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv13 = t14
    return retv13
}

func main() {
    main0()
}
