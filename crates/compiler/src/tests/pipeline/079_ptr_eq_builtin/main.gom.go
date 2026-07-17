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
    var t64 bool = ptr_eq__Ref_5int32(a__0, b__1)
    println__T_bool(t64)
    var t65 bool = ptr_eq__Ref_5int32(a__0, c__2)
    println__T_bool(t65)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv67 *ref_int32_x
    var t68 *ref_int32_x = ref__Ref_5int32(value__204)
    retv67 = t68
    return retv67
}

func println__T_bool(value__1 bool) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv73 string
    var t74 string = _goml_runtime_core_bool_to_string(self__36)
    retv73 = t74
    return retv73
}

func main() {
    main0()
}
