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
    Loop_loop137:
    for {
        var t148 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t149 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t148, 0)
        var jp139 bool
        if t149 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 1)
            jp139 = true
        } else {
            var t152 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t153 bool = t152 < 4
            var jp151 bool
            if t153 {
                jp151 = true
            } else {
                jp151 = false
            }
            jp139 = jp151
        }
        if jp139 {
            var t140 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
            var t141 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t142 int = t140 + t141
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__1, t142)
            var t146 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t147 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t146, 1)
            if t147 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 2)
                continue
            } else {
                var t144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t145 int = t144 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t145)
                continue
            }
        } else {
            break Loop_loop137
        }
    }
    var t125 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t125)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop128:
    for {
        var mtmp115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
        var jp130 bool
        switch mtmp115 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 1)
            jp130 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 2)
            jp130 = true
        case 2:
            jp130 = true
        default:
            jp130 = false
        }
        if jp130 {
            var t131 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
            var t132 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t133 int = t131 + t132
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total2__3, t133)
            var t135 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t136 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t135, 2)
            if t136 {
                break Loop_loop128
            } else {
                continue
            }
        } else {
            break Loop_loop128
        }
    }
    var t127 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
    println__T_int(t127)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv155 *ref_int_x
    var t156 *ref_int_x = ref__Ref_3int(value__207)
    retv155 = t156
    return retv155
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv158 int
    var t159 int = ref_get__Ref_3int(self__208)
    retv158 = t159
    return retv158
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv161 bool
    var t162 bool = self__59 == other__60
    retv161 = t162
    return retv161
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int_to_string(self__40)
    retv169 = t170
    return retv169
}

func main() {
    main0()
}
