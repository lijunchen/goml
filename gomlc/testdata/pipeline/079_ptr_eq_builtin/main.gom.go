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
    var t111 bool = ptr_eq__Ref_3int(a__0, b__1)
    println__T_bool(t111)
    var t112 bool = ptr_eq__Ref_3int(a__0, c__2)
    println__T_bool(t112)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv114 *ref_int_x
    var t115 *ref_int_x = ref__Ref_3int(value__207)
    retv114 = t115
    return retv114
}

func println__T_bool(value__1 bool) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv120 string
    var t121 string = _goml_runtime_core_bool_to_string(self__37)
    retv120 = t121
    return retv120
}

func main() {
    main0()
}
