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
    Loop_loop174:
    for {
        var t175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t176 bool = t175 < 5
        if t176 {
            var t177 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t178 int = t177 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t178)
            var t183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t184 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t183, 3)
            var jp180 int
            if t184 {
                continue
            } else {
                var t185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                jp180 = t185
                var cur__2 int = jp180
                var t181 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                var t182 int = t181 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t182)
                continue
            }
        } else {
            break Loop_loop174
        }
    }
    var t163 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t163)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop166:
    for {
        if true {
            var t167 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t168 int = t167 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t168)
            var mtmp157 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var jp170 int
            switch mtmp157 {
            case 5:
                break Loop_loop166
            default:
                var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
                jp170 = t173
                var cur__5 int = jp170
                var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
                var t172 int = t171 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__4, t172)
                continue
            }
        } else {
            break Loop_loop166
        }
    }
    var t165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
    println__T_int(t165)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv187 *ref_int_x
    var t188 *ref_int_x = ref__Ref_3int(value__207)
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv190 int
    var t191 int = ref_get__Ref_3int(self__208)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv195 bool
    var t196 bool = self__59 == other__60
    retv195 = t196
    return retv195
}

func println__T_int(value__1 int) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv201 string
    var t202 string = _goml_runtime_core_int_to_string(self__40)
    retv201 = t202
    return retv201
}

func main() {
    main0()
}
