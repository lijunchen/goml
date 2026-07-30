package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop116:
    for {
        var t117 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t118 bool = t117 < 7
        if t118 {
            var cur__2 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t119 int = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t119)
            var t123 bool = cur__2 < 5
            if t123 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t121 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t122 int = t121 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t122)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop116
                default:
                    var t121 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t122 int = t121 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t122)
                    continue
                }
            }
        } else {
            break Loop_loop116
        }
    }
    var t115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t115)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv127 *ref_int_x
    var t128 *ref_int_x = ref__Ref_3int(value__207)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv130 int
    var t131 int = ref_get__Ref_3int(self__208)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv138 string
    var t139 string = _goml_runtime_core_int_to_string(self__40)
    retv138 = t139
    return retv138
}

func main() {
    main0()
}
