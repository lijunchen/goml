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
    Loop_loop90:
    for {
        var t91 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t92 bool = t91 < 5
        if t92 {
            var t93 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t94 int = t93 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t94)
            var t99 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t100 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t99, 3)
            var jp96 int
            if t100 {
                continue
            } else {
                var t101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                jp96 = t101
                var cur__2 int = jp96
                var t97 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                var t98 int = t97 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t98)
                continue
            }
        } else {
            break Loop_loop90
        }
    }
    var t79 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t79)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop82:
    for {
        if true {
            var t83 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t84 int = t83 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t84)
            var mtmp73 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var jp86 int
            switch mtmp73 {
            case 5:
                break Loop_loop82
            default:
                var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
                jp86 = t89
                var cur__5 int = jp86
                var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
                var t88 int = t87 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__4, t88)
                continue
            }
        } else {
            break Loop_loop82
        }
    }
    var t81 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
    println__T_int(t81)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv103 *ref_int_x
    var t104 *ref_int_x = ref__Ref_3int(value__207)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv106 int
    var t107 int = ref_get__Ref_3int(self__208)
    retv106 = t107
    return retv106
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv111 bool
    var t112 bool = self__59 == other__60
    retv111 = t112
    return retv111
}

func println__T_int(value__1 int) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int_to_string(self__40)
    retv117 = t118
    return retv117
}

func main() {
    main0()
}
