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
    Loop_loop155:
    for {
        var t161 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t162 bool = t161 < 3
        var jp157 bool
        if t162 {
            jp157 = true
        } else {
            jp157 = false
        }
        if jp157 {
            var t158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            println__T_int(t158)
            var t159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t160 int = t159 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t160)
            continue
        } else {
            break Loop_loop155
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop138:
    for {
        var t146 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
        var t147 bool = t146 < 4
        var jp140 bool
        if t147 {
            var t150 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t151 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t150, 1)
            var jp149 bool
            if t151 {
                jp149 = true
            } else {
                var t152 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
                var t153 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t152, 3)
                var t154 bool = !t153
                jp149 = t154
            }
            jp140 = jp149
        } else {
            jp140 = false
        }
        if jp140 {
            var t141 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
            var t142 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t143 int = t141 + t142
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__2, t143)
            var t144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t145 int = t144 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__1, t145)
            continue
        } else {
            break Loop_loop138
        }
    }
    var t123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t123)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop126:
    for {
        var mtmp115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
        var jp128 bool
        switch mtmp115 {
        case 0:
            jp128 = true
        case 1:
            var t136 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t137 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t136, 0)
            var jp135 bool
            if t137 {
                jp135 = true
            } else {
                jp135 = false
            }
            jp128 = jp135
        case 2:
            jp128 = true
        default:
            jp128 = false
        }
        if jp128 {
            var t129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t130 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t131 int = t129 + t130
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__4, t131)
            var t132 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t133 int = t132 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(k__3, t133)
            continue
        } else {
            break Loop_loop126
        }
    }
    var t125 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t125)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv164 *ref_int_x
    var t165 *ref_int_x = ref__Ref_3int(value__207)
    retv164 = t165
    return retv164
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv167 int
    var t168 int = ref_get__Ref_3int(self__208)
    retv167 = t168
    return retv167
}

func println__T_int(value__1 int) struct{} {
    var t170 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv175 bool
    var t176 bool = self__59 == other__60
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int_to_string(self__40)
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
