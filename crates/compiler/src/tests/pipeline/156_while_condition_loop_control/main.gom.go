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
    Loop_loop33:
    for {
        var t44 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t45 bool = t44 == 0
        var jp35 bool
        if t45 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 1)
            jp35 = true
        } else {
            var t48 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t49 bool = t48 < 4
            var jp47 bool
            if t49 {
                jp47 = true
            } else {
                jp47 = false
            }
            jp35 = jp47
        }
        if jp35 {
            var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
            var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t38 int32 = t36 + t37
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__1, t38)
            var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t43 bool = t42 == 1
            if t43 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 2)
                continue
            } else {
                var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t41 int32 = t40 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t41)
                continue
            }
        } else {
            break Loop_loop33
        }
    }
    var t21 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
    println__T_int32(t21)
    var j__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total2__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop24:
    for {
        var mtmp11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
        var jp26 bool
        switch mtmp11 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 1)
            jp26 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 2)
            jp26 = true
        case 2:
            jp26 = true
        default:
            jp26 = false
        }
        if jp26 {
            var t27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
            var t28 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t29 int32 = t27 + t28
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total2__3, t29)
            var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t32 bool = t31 == 2
            if t32 {
                break Loop_loop24
            } else {
                continue
            }
        } else {
            break Loop_loop24
        }
    }
    var t23 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
    println__T_int32(t23)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv51 *ref_int32_x
    var t52 *ref_int32_x = ref__Ref_5int32(value__102)
    retv51 = t52
    return retv51
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv54 int32
    var t55 int32 = ref_get__Ref_5int32(self__103)
    retv54 = t55
    return retv54
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t59 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t59)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv62 string
    var t63 string = _goml_runtime_core_int32_to_string(self__13)
    retv62 = t63
    return retv62
}

func main() {
    main0()
}
