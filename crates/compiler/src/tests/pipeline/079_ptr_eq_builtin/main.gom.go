package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ptr_eq__Ref_5int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func main0() struct{} {
    var a__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var b__1 *ref_int32_x = a__0
    var c__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t10 bool = ptr_eq__Ref_5int32(a__0, b__1)
    println__T_bool(t10)
    var t11 bool = ptr_eq__Ref_5int32(a__0, c__2)
    println__T_bool(t11)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv13 *ref_int32_x
    var t14 *ref_int32_x = ref__Ref_5int32(value__114)
    retv13 = t14
    return retv13
}

func println__T_bool(value__1 bool) struct{} {
    var t16 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t16)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv19 string
    var t20 string = _goml_runtime_core_bool_to_string(self__8)
    retv19 = t20
    return retv19
}

func main() {
    main0()
}
