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
    Loop_loop30:
    for {
        var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t32 bool = t31 < 7
        if t32 {
            var cur__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t33 int32 = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t33)
            var t37 bool = cur__2 < 5
            if t37 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t36 int32 = t35 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t36)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop30
                default:
                    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t36 int32 = t35 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t36)
                    continue
                }
            }
        } else {
            break Loop_loop30
        }
    }
    var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t29)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv41 *ref_int32_x
    var t42 *ref_int32_x = ref__Ref_5int32(value__137)
    retv41 = t42
    return retv41
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv44 int32
    var t45 int32 = ref_get__Ref_5int32(self__138)
    retv44 = t45
    return retv44
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t49 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv52 string
    var t53 string = _goml_runtime_core_int32_to_string(self__13)
    retv52 = t53
    return retv52
}

func main() {
    main0()
}
