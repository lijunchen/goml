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
    Loop_loop184:
    for {
        var t195 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t196 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t195, 0)
        var jp186 bool
        if t196 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 1)
            jp186 = true
        } else {
            var t199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t200 bool = t199 < 4
            if t200 {
                jp186 = true
            } else {
                jp186 = false
            }
        }
        if jp186 {
            var t187 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
            var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t189 int = t187 + t188
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__1, t189)
            var t193 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t194 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t193, 1)
            if t194 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 2)
                continue
            } else {
                var t191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t192 int = t191 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t192)
                continue
            }
        } else {
            break Loop_loop184
        }
    }
    var t172 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t172)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop175:
    for {
        var mtmp162 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
        var jp177 bool
        switch mtmp162 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 1)
            jp177 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 2)
            jp177 = true
        case 2:
            jp177 = true
        default:
            jp177 = false
        }
        if jp177 {
            var t178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
            var t179 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t180 int = t178 + t179
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total2__3, t180)
            var t182 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t183 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t182, 2)
            if t183 {
                break Loop_loop175
            } else {
                continue
            }
        } else {
            break Loop_loop175
        }
    }
    var t174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
    println__T_int(t174)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t203 *ref_int_x = ref__Ref_3int(value__207)
    return t203
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t206 int = ref_get__Ref_3int(self__208)
    return t206
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t209 bool = self__59 == other__60
    return t209
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t213 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t217 string = _goml_runtime_core_int_to_string(self__40)
    return t217
}

func main() {
    main0()
}
