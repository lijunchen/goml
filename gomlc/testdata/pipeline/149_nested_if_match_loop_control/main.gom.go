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
    Loop_loop160:
    for {
        var t161 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t162 bool = t161 < 7
        if t162 {
            var cur__2 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t163 int = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t163)
            var t167 bool = cur__2 < 5
            if t167 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t166 int = t165 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t166)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop160
                default:
                    var t165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t166 int = t165 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t166)
                    continue
                }
            }
        } else {
            break Loop_loop160
        }
    }
    var t159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t159)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv171 *ref_int_x
    var t172 *ref_int_x = ref__Ref_3int(value__207)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv174 int
    var t175 int = ref_get__Ref_3int(self__208)
    retv174 = t175
    return retv174
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv182 string
    var t183 string = _goml_runtime_core_int_to_string(self__40)
    retv182 = t183
    return retv182
}

func main() {
    main0()
}
