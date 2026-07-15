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
    Loop_loop29:
    for {
        var t30 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t31 bool = t30 < 10
        if t31 {
            var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t33 int32 = t32 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t33)
            var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t37 bool = t36 == 5
            if t37 {
                continue
            } else {
                var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                println__T_int32(t35)
                continue
            }
        } else {
            break Loop_loop29
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv39 *ref_int32_x
    var t40 *ref_int32_x = ref__Ref_5int32(value__140)
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv42 int32
    var t43 int32 = ref_get__Ref_5int32(self__141)
    retv42 = t43
    return retv42
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv53 string
    var t54 string = _goml_runtime_core_int32_to_string(self__13)
    retv53 = t54
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv56 string
    retv56 = self__9
    return retv56
}

func main() {
    main0()
}
