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
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop93:
    for {
        var t104 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t105 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t104, 0)
        var jp95 bool
        if t105 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 1)
            jp95 = true
        } else {
            var t108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t109 bool = t108 < 4
            var jp107 bool
            if t109 {
                jp107 = true
            } else {
                jp107 = false
            }
            jp95 = jp107
        }
        if jp95 {
            var t96 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
            var t97 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t98 int = t96 + t97
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__1, t98)
            var t102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t103 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t102, 1)
            if t103 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 2)
                continue
            } else {
                var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t101 int = t100 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t101)
                continue
            }
        } else {
            break Loop_loop93
        }
    }
    var t81 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t81)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop84:
    for {
        var mtmp71 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
        var jp86 bool
        switch mtmp71 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 1)
            jp86 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 2)
            jp86 = true
        case 2:
            jp86 = true
        default:
            jp86 = false
        }
        if jp86 {
            var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
            var t88 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t89 int = t87 + t88
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total2__3, t89)
            var t91 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t92 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t91, 2)
            if t92 {
                break Loop_loop84
            } else {
                continue
            }
        } else {
            break Loop_loop84
        }
    }
    var t83 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
    println__T_int(t83)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv111 *ref_int_x
    var t112 *ref_int_x = ref__Ref_3int(value__209)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv114 int
    var t115 int = ref_get__Ref_3int(self__210)
    retv114 = t115
    return retv114
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv117 bool
    var t118 bool = self__59 == other__60
    retv117 = t118
    return retv117
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int_to_string(self__40)
    retv125 = t126
    return retv125
}

func main() {
    main0()
}
