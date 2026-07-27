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
    Loop_loop76:
    for {
        var t77 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t78 bool = t77 < 20
        if t78 {
            var t79 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t80 int = t79 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t80)
            var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t86 bool = t85 > 5
            if t86 {
                break Loop_loop76
            } else {
                var t82 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t83 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t84 int = t82 + t83
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t84)
                continue
            }
        } else {
            break Loop_loop76
        }
    }
    print__T_string("sum: ")
    var t74 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t74)
    print__T_string("i at break: ")
    var t75 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
    println__T_int(t75)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv88 *ref_int_x
    var t89 *ref_int_x = ref__Ref_3int(value__209)
    retv88 = t89
    return retv88
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv91 int
    var t92 int = ref_get__Ref_3int(self__210)
    retv91 = t92
    return retv91
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t96)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv102 string
    retv102 = self__38
    return retv102
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int_to_string(self__40)
    retv104 = t105
    return retv104
}

func main() {
    main0()
}
