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
    Loop_loop181:
    for {
        var t192 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t193 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t192, 0)
        var jp183 bool
        if t193 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 1)
            jp183 = true
        } else {
            var t196 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t197 bool = t196 < 4
            var jp195 bool
            if t197 {
                jp195 = true
            } else {
                jp195 = false
            }
            jp183 = jp195
        }
        if jp183 {
            var t184 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
            var t185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t186 int = t184 + t185
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__1, t186)
            var t190 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t191 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t190, 1)
            if t191 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 2)
                continue
            } else {
                var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t189 int = t188 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t189)
                continue
            }
        } else {
            break Loop_loop181
        }
    }
    var t169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t169)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop172:
    for {
        var mtmp159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
        var jp174 bool
        switch mtmp159 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 1)
            jp174 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 2)
            jp174 = true
        case 2:
            jp174 = true
        default:
            jp174 = false
        }
        if jp174 {
            var t175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t177 int = t175 + t176
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total2__3, t177)
            var t179 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t180 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t179, 2)
            if t180 {
                break Loop_loop172
            } else {
                continue
            }
        } else {
            break Loop_loop172
        }
    }
    var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
    println__T_int(t171)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv199 *ref_int_x
    var t200 *ref_int_x = ref__Ref_3int(value__207)
    retv199 = t200
    return retv199
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv202 int
    var t203 int = ref_get__Ref_3int(self__208)
    retv202 = t203
    return retv202
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv205 bool
    var t206 bool = self__59 == other__60
    retv205 = t206
    return retv205
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t210 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv213 string
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    retv213 = t214
    return retv213
}

func main() {
    main0()
}
