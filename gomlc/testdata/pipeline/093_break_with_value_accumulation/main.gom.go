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
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop167:
    for {
        var t168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t169 bool = t168 < 20
        if t169 {
            var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t171 int = t170 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t171)
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t177 bool = t176 > 5
            if t177 {
                break Loop_loop167
            } else {
                var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t175 int = t173 + t174
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t175)
                continue
            }
        } else {
            break Loop_loop167
        }
    }
    print__T_string("sum: ")
    var t165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t165)
    print__T_string("i at break: ")
    var t166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
    println__T_int(t166)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t180 *ref_int_x = ref__Ref_3int(value__207)
    return t180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t183 int = ref_get__Ref_3int(self__208)
    return t183
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t187)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t196 string = _goml_runtime_core_int_to_string(self__40)
    return t196
}

func main() {
    main0()
}
