package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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
    Loop_loop22:
    for {
        var t23 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t24 bool = t23 < 5
        if t24 {
            var t25 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t26 int32 = t25 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t26)
            var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t32 bool = t31 == 3
            var jp28 int32
            if t32 {
                continue
            } else {
                var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                jp28 = t33
                var cur__2 int32 = jp28
                var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                var t30 int32 = t29 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t30)
                continue
            }
        } else {
            break Loop_loop22
        }
    }
    var t11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t11)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop14:
    for {
        if true {
            var t15 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t16 int32 = t15 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t16)
            var mtmp5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var jp18 int32
            switch mtmp5 {
            case 5:
                break Loop_loop14
            default:
                var t21 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
                jp18 = t21
                var cur__5 int32 = jp18
                var t19 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
                var t20 int32 = t19 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__4, t20)
                continue
            }
        } else {
            break Loop_loop14
        }
    }
    var t13 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
    println__T_int32(t13)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__93 int32) *ref_int32_x {
    var retv35 *ref_int32_x
    var t36 *ref_int32_x = ref__Ref_5int32(value__93)
    retv35 = t36
    return retv35
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__94 *ref_int32_x) int32 {
    var retv38 int32
    var t39 int32 = ref_get__Ref_5int32(self__94)
    retv38 = t39
    return retv38
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t43 string = int32_to_string(value__1)
    string_println(t43)
    return struct{}{}
}

func main() {
    main0()
}
