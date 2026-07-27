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
    Loop_loop86:
    for {
        var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t88 bool = t87 < 5
        if t88 {
            var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t90 int = t89 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t90)
            var t95 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t96 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t95, 3)
            var jp92 int
            if t96 {
                continue
            } else {
                var t97 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                jp92 = t97
                var cur__2 int = jp92
                var t93 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                var t94 int = t93 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t94)
                continue
            }
        } else {
            break Loop_loop86
        }
    }
    var t75 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t75)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop78:
    for {
        if true {
            var t79 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t80 int = t79 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t80)
            var mtmp69 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var jp82 int
            switch mtmp69 {
            case 5:
                break Loop_loop78
            default:
                var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
                jp82 = t85
                var cur__5 int = jp82
                var t83 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
                var t84 int = t83 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__4, t84)
                continue
            }
        } else {
            break Loop_loop78
        }
    }
    var t77 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
    println__T_int(t77)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv99 *ref_int_x
    var t100 *ref_int_x = ref__Ref_3int(value__209)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv102 int
    var t103 int = ref_get__Ref_3int(self__210)
    retv102 = t103
    return retv102
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv107 bool
    var t108 bool = self__59 == other__60
    retv107 = t108
    return retv107
}

func println__T_int(value__1 int) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int_to_string(self__40)
    retv113 = t114
    return retv113
}

func main() {
    main0()
}
