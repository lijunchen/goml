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
    Loop_loop120:
    for {
        var t121 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t122 bool = t121 < 20
        if t122 {
            var t123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t124 int = t123 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t124)
            var t129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t130 bool = t129 > 5
            if t130 {
                break Loop_loop120
            } else {
                var t126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t127 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t128 int = t126 + t127
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t128)
                continue
            }
        } else {
            break Loop_loop120
        }
    }
    print__T_string("sum: ")
    var t118 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t118)
    print__T_string("i at break: ")
    var t119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
    println__T_int(t119)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv132 *ref_int_x
    var t133 *ref_int_x = ref__Ref_3int(value__207)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv135 int
    var t136 int = ref_get__Ref_3int(self__208)
    retv135 = t136
    return retv135
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t140)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv146 string
    retv146 = self__38
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv148 string
    var t149 string = _goml_runtime_core_int_to_string(self__40)
    retv148 = t149
    return retv148
}

func main() {
    main0()
}
