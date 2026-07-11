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
    Loop_loop26:
    for {
        var t27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t28 bool = t27 < 5
        if t28 {
            var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t30 int32 = t29 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t30)
            var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t36 bool = t35 == 3
            var jp32 int32
            if t36 {
                continue
            } else {
                var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                jp32 = t37
                var cur__2 int32 = jp32
                var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                var t34 int32 = t33 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t34)
                continue
            }
        } else {
            break Loop_loop26
        }
    }
    var t15 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t15)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop18:
    for {
        if true {
            var t19 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t20 int32 = t19 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t20)
            var mtmp9 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var jp22 int32
            switch mtmp9 {
            case 5:
                break Loop_loop18
            default:
                var t25 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
                jp22 = t25
                var cur__5 int32 = jp22
                var t23 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
                var t24 int32 = t23 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__4, t24)
                continue
            }
        } else {
            break Loop_loop18
        }
    }
    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
    println__T_int32(t17)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv39 *ref_int32_x
    var t40 *ref_int32_x = ref__Ref_5int32(value__102)
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv42 int32
    var t43 int32 = ref_get__Ref_5int32(self__103)
    retv42 = t43
    return retv42
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv50 string
    var t51 string = _goml_runtime_core_int32_to_string(self__13)
    retv50 = t51
    return retv50
}

func main() {
    main0()
}
