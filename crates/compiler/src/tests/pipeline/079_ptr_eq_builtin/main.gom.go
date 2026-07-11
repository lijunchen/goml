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
    var t7 bool = ptr_eq__Ref_5int32(a__0, b__1)
    println__T_bool(t7)
    var t8 bool = ptr_eq__Ref_5int32(a__0, c__2)
    println__T_bool(t8)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv10 *ref_int32_x
    var t11 *ref_int32_x = ref__Ref_5int32(value__102)
    retv10 = t11
    return retv10
}

func println__T_bool(value__1 bool) struct{} {
    var t13 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t13)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv16 string
    var t17 string = _goml_runtime_core_bool_to_string(self__8)
    retv16 = t17
    return retv16
}

func main() {
    main0()
}
