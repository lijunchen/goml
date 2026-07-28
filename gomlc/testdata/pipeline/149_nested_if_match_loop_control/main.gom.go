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
    Loop_loop72:
    for {
        var t73 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t74 bool = t73 < 7
        if t74 {
            var cur__2 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t75 int = cur__2 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t75)
            var t79 bool = cur__2 < 5
            if t79 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t77 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t78 int = t77 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t78)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop72
                default:
                    var t77 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                    var t78 int = t77 + cur__2
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t78)
                    continue
                }
            }
        } else {
            break Loop_loop72
        }
    }
    var t71 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t71)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv83 *ref_int_x
    var t84 *ref_int_x = ref__Ref_3int(value__209)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv86 int
    var t87 int = ref_get__Ref_3int(self__210)
    retv86 = t87
    return retv86
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int_to_string(self__40)
    retv94 = t95
    return retv94
}

func main() {
    main0()
}
