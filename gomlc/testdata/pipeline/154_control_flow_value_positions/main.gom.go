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
    var sum__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop177:
    for {
        var t178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t179 bool = t178 < 5
        if t179 {
            var t180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t181 int = t180 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t181)
            var t186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t187 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t186, 3)
            var jp183 int
            if t187 {
                continue
            } else {
                var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                jp183 = t188
                var t184 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                var t185 int = t184 + jp183
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t185)
                continue
            }
        } else {
            break Loop_loop177
        }
    }
    var t166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t166)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop169:
    for {
        var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t171 int = t170 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t171)
        var mtmp160 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var jp173 int
        switch mtmp160 {
        case 5:
            break Loop_loop169
        default:
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            jp173 = t176
            var t174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
            var t175 int = t174 + jp173
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__4, t175)
            continue
        }
    }
    var t168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
    println__T_int(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t191 *ref_int_x = ref__Ref_3int(value__207)
    return t191
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t194 int = ref_get__Ref_3int(self__208)
    return t194
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t199 bool = self__59 == other__60
    return t199
}

func println__T_int(value__1 int) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t205 string = _goml_runtime_core_int_to_string(self__40)
    return t205
}

func main() {
    main0()
}
