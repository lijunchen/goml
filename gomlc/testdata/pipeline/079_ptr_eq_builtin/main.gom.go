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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ptr_eq__Ref_3int(a *ref_int_x, b *ref_int_x) bool {
    return a == b
}

func main0() struct{} {
    var a__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    var b__1 *ref_int_x = a__0
    var c__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    var t155 bool = ptr_eq__Ref_3int(a__0, b__1)
    println__T_bool(t155)
    var t156 bool = ptr_eq__Ref_3int(a__0, c__2)
    println__T_bool(t156)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv158 *ref_int_x
    var t159 *ref_int_x = ref__Ref_3int(value__207)
    retv158 = t159
    return retv158
}

func println__T_bool(value__1 bool) struct{} {
    var t161 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv164 string
    var t165 string = _goml_runtime_core_bool_to_string(self__37)
    retv164 = t165
    return retv164
}

func main() {
    main0()
}
