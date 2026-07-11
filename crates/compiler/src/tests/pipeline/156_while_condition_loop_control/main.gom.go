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
    var total__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop36:
    for {
        var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t48 bool = t47 == 0
        var jp38 bool
        if t48 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 1)
            jp38 = true
        } else {
            var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t52 bool = t51 < 4
            var jp50 bool
            if t52 {
                jp50 = true
            } else {
                jp50 = false
            }
            jp38 = jp50
        }
        if jp38 {
            var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
            var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t41 int32 = t39 + t40
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__1, t41)
            var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t46 bool = t45 == 1
            if t46 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 2)
                continue
            } else {
                var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t44 int32 = t43 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t44)
                continue
            }
        } else {
            break Loop_loop36
        }
    }
    var t24 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
    println__T_int32(t24)
    var j__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total2__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop27:
    for {
        var mtmp14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
        var jp29 bool
        switch mtmp14 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 1)
            jp29 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 2)
            jp29 = true
        case 2:
            jp29 = true
        default:
            jp29 = false
        }
        if jp29 {
            var t30 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
            var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t32 int32 = t30 + t31
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total2__3, t32)
            var t34 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t35 bool = t34 == 2
            if t35 {
                break Loop_loop27
            } else {
                continue
            }
        } else {
            break Loop_loop27
        }
    }
    var t26 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
    println__T_int32(t26)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv54 *ref_int32_x
    var t55 *ref_int32_x = ref__Ref_5int32(value__114)
    retv54 = t55
    return retv54
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv57 int32
    var t58 int32 = ref_get__Ref_5int32(self__115)
    retv57 = t58
    return retv57
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv65 string
    var t66 string = _goml_runtime_core_int32_to_string(self__13)
    retv65 = t66
    return retv65
}

func main() {
    main0()
}
