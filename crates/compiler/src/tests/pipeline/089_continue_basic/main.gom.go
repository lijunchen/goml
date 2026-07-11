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
    Loop_loop11:
    for {
        var t12 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t13 bool = t12 < 10
        if t13 {
            var t14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t15 int32 = t14 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t15)
            var t18 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t19 bool = t18 == 5
            if t19 {
                continue
            } else {
                var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                println__T_int32(t17)
                continue
            }
        } else {
            break Loop_loop11
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv21 *ref_int32_x
    var t22 *ref_int32_x = ref__Ref_5int32(value__102)
    retv21 = t22
    return retv21
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv24 int32
    var t25 int32 = ref_get__Ref_5int32(self__103)
    retv24 = t25
    return retv24
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t29 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t29)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv35 string
    var t36 string = _goml_runtime_core_int32_to_string(self__13)
    retv35 = t36
    return retv35
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv38 string
    retv38 = self__9
    return retv38
}

func main() {
    main0()
}
