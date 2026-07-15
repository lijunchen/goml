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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var i__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop30:
    for {
        var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t32 bool = t31 < 8
        if t32 {
            var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t34 int32 = t33 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t34)
            var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t41 bool = t40 == 3
            if t41 {
                continue
            } else {
                var t38 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t39 bool = t38 == 6
                if t39 {
                    continue
                } else {
                    var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                    println__T_int32(t37)
                    continue
                }
            }
        } else {
            break Loop_loop30
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv43 *ref_int32_x
    var t44 *ref_int32_x = ref__Ref_5int32(value__140)
    retv43 = t44
    return retv43
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv46 int32
    var t47 int32 = ref_get__Ref_5int32(self__141)
    retv46 = t47
    return retv46
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv57 string
    var t58 string = _goml_runtime_core_int32_to_string(self__13)
    retv57 = t58
    return retv57
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv60 string
    retv60 = self__9
    return retv60
}

func main() {
    main0()
}
