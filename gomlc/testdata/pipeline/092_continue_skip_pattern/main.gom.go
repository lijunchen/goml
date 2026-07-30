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
    Loop_loop116:
    for {
        var t117 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t118 bool = t117 < 8
        if t118 {
            var t119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t120 int = t119 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t120)
            var t126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t127 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t126, 3)
            if t127 {
                continue
            } else {
                var t124 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t125 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t124, 6)
                if t125 {
                    continue
                } else {
                    var t123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                    println__T_int(t123)
                    continue
                }
            }
        } else {
            break Loop_loop116
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv129 *ref_int_x
    var t130 *ref_int_x = ref__Ref_3int(value__207)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv132 int
    var t133 int = ref_get__Ref_3int(self__208)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv137 bool
    var t138 bool = self__59 == other__60
    retv137 = t138
    return retv137
}

func println__T_int(value__1 int) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv146 string
    var t147 string = _goml_runtime_core_int_to_string(self__40)
    retv146 = t147
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv149 string
    retv149 = self__38
    return retv149
}

func main() {
    main0()
}
