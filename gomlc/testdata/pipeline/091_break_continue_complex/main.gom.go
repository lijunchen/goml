package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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
    var sum__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop179:
    for {
        var t180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t181 bool = t180 <= 100
        if t181 {
            var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t189 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t188, 50)
            if t189 {
                break Loop_loop179
            } else {
                var t183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t184 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t185 int = t183 + t184
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t185)
                var t186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t187 int = t186 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t187)
                continue
            }
        } else {
            break Loop_loop179
        }
    }
    print__T_string("sum up to break: ")
    var t166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t166)
    var even_sum__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop169:
    for {
        var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t171 bool = t170 <= 10
        if t171 {
            var cur__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t172 int = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t172)
            var t174 int = cur__4 / 2
            var t175 int = t174 * 2
            var t176 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(cur__4, t175)
            if t176 {
                var t177 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
                var t178 int = t177 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(even_sum__2, t178)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop169
        }
    }
    print__T_string("even sum: ")
    var t168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
    println__T_int(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv191 *ref_int_x
    var t192 *ref_int_x = ref__Ref_3int(value__207)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv194 int
    var t195 int = ref_get__Ref_3int(self__208)
    retv194 = t195
    return retv194
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv197 bool
    var t198 bool = self__59 == other__60
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t202)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv208 string
    retv208 = self__38
    return retv208
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv210 string
    var t211 string = _goml_runtime_core_int_to_string(self__40)
    retv210 = t211
    return retv210
}

func main() {
    main0()
}
