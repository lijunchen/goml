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
    Loop_loop163:
    for {
        var t164 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t165 bool = t164 < 8
        if t165 {
            var t166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t167 int = t166 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t167)
            var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t174 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t173, 3)
            if t174 {
                continue
            } else {
                var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t172 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t171, 6)
                if t172 {
                    continue
                } else {
                    var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                    println__T_int(t170)
                    continue
                }
            }
        } else {
            break Loop_loop163
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t177 *ref_int_x = ref__Ref_3int(value__207)
    return t177
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t180 int = ref_get__Ref_3int(self__208)
    return t180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t185 bool = self__59 == other__60
    return t185
}

func println__T_int(value__1 int) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t194 string = _goml_runtime_core_int_to_string(self__40)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
