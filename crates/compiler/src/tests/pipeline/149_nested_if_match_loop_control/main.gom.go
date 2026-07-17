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
    Loop_loop69:
    for {
        var t70 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t71 bool = t70 < 7
        if t71 {
            var cur__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t72 int32 = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t72)
            var t76 bool = cur__2 < 5
            if t76 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t75 int32 = t74 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t75)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop69
                default:
                    var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t75 int32 = t74 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t75)
                    continue
                }
            }
        } else {
            break Loop_loop69
        }
    }
    var t68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t68)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv80 *ref_int32_x
    var t81 *ref_int32_x = ref__Ref_5int32(value__204)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv83 int32
    var t84 int32 = ref_get__Ref_5int32(self__205)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__41)
    retv91 = t92
    return retv91
}

func main() {
    main0()
}
