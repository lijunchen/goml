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
    Loop_loop51:
    for {
        var t62 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t63 bool = t62 == 0
        var jp53 bool
        if t63 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 1)
            jp53 = true
        } else {
            var t66 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t67 bool = t66 < 4
            var jp65 bool
            if t67 {
                jp65 = true
            } else {
                jp65 = false
            }
            jp53 = jp65
        }
        if jp53 {
            var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
            var t55 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t56 int32 = t54 + t55
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__1, t56)
            var t60 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t61 bool = t60 == 1
            if t61 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 2)
                continue
            } else {
                var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t59 int32 = t58 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t59)
                continue
            }
        } else {
            break Loop_loop51
        }
    }
    var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
    println__T_int32(t39)
    var j__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total2__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop42:
    for {
        var mtmp29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
        var jp44 bool
        switch mtmp29 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 1)
            jp44 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 2)
            jp44 = true
        case 2:
            jp44 = true
        default:
            jp44 = false
        }
        if jp44 {
            var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
            var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t47 int32 = t45 + t46
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total2__3, t47)
            var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t50 bool = t49 == 2
            if t50 {
                break Loop_loop42
            } else {
                continue
            }
        } else {
            break Loop_loop42
        }
    }
    var t41 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
    println__T_int32(t41)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv69 *ref_int32_x
    var t70 *ref_int32_x = ref__Ref_5int32(value__140)
    retv69 = t70
    return retv69
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv72 int32
    var t73 int32 = ref_get__Ref_5int32(self__141)
    retv72 = t73
    return retv72
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int32_to_string(self__13)
    retv80 = t81
    return retv80
}

func main() {
    main0()
}
