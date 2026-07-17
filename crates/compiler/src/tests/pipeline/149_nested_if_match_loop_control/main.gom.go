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
    Loop_loop66:
    for {
        var t67 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t68 bool = t67 < 7
        if t68 {
            var cur__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t69 int32 = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t69)
            var t73 bool = cur__2 < 5
            if t73 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t72 int32 = t71 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t72)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop66
                default:
                    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                    var t72 int32 = t71 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t72)
                    continue
                }
            }
        } else {
            break Loop_loop66
        }
    }
    var t65 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t65)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv77 *ref_int32_x
    var t78 *ref_int32_x = ref__Ref_5int32(value__200)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv80 int32
    var t81 int32 = ref_get__Ref_5int32(self__201)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv88 string
    var t89 string = _goml_runtime_core_int32_to_string(self__38)
    retv88 = t89
    return retv88
}

func main() {
    main0()
}
