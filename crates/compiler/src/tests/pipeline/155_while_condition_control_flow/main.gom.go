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
    Loop_loop50:
    for {
        var t56 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t57 bool = t56 < 3
        var jp52 bool
        if t57 {
            jp52 = true
        } else {
            jp52 = false
        }
        if jp52 {
            var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            println__T_int32(t53)
            var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t55 int32 = t54 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t55)
            continue
        } else {
            break Loop_loop50
        }
    }
    var j__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop34:
    for {
        var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
        var t43 bool = t42 < 4
        var jp36 bool
        if t43 {
            var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t47 bool = t46 == 1
            var jp45 bool
            if t47 {
                jp45 = true
            } else {
                var t48 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
                var t49 bool = t48 != 3
                jp45 = t49
            }
            jp36 = jp45
        } else {
            jp36 = false
        }
        if jp36 {
            var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
            var t38 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t39 int32 = t37 + t38
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t39)
            var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t41 int32 = t40 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__1, t41)
            continue
        } else {
            break Loop_loop34
        }
    }
    var t19 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    println__T_int32(t19)
    var k__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var sum__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop22:
    for {
        var mtmp11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
        var jp24 bool
        switch mtmp11 {
        case 0:
            jp24 = true
        case 1:
            var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t33 bool = t32 == 0
            var jp31 bool
            if t33 {
                jp31 = true
            } else {
                jp31 = false
            }
            jp24 = jp31
        case 2:
            jp24 = true
        default:
            jp24 = false
        }
        if jp24 {
            var t25 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t26 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t27 int32 = t25 + t26
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__4, t27)
            var t28 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t29 int32 = t28 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(k__3, t29)
            continue
        } else {
            break Loop_loop22
        }
    }
    var t21 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
    println__T_int32(t21)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv59 *ref_int32_x
    var t60 *ref_int32_x = ref__Ref_5int32(value__102)
    retv59 = t60
    return retv59
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv62 int32
    var t63 int32 = ref_get__Ref_5int32(self__103)
    retv62 = t63
    return retv62
}

func println__T_int32(value__1 int32) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__13)
    retv70 = t71
    return retv70
}

func main() {
    main0()
}
