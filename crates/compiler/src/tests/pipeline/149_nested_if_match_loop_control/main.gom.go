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
    var sum__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop12:
    for {
        var t13 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t14 bool = t13 < 7
        if t14 {
            var cur__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t15 int32 = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t15)
            var t19 bool = cur__2 < 5
            if t19 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t18 int32 = t17 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t18)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop12
                default:
                    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t18 int32 = t17 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t18)
                    continue
                }
            }
        } else {
            break Loop_loop12
        }
    }
    var t11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t11)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv23 *ref_int32_x
    var t24 *ref_int32_x = ref__Ref_5int32(value__102)
    retv23 = t24
    return retv23
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv26 int32
    var t27 int32 = ref_get__Ref_5int32(self__103)
    retv26 = t27
    return retv26
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv34 string
    var t35 string = _goml_runtime_core_int32_to_string(self__13)
    retv34 = t35
    return retv34
}

func main() {
    main0()
}
