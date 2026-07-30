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
    Loop_loop115:
    for {
        var t121 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t122 bool = t121 < 3
        var jp117 bool
        if t122 {
            jp117 = true
        } else {
            jp117 = false
        }
        if jp117 {
            var t118 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            println__T_int(t118)
            var t119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t120 int = t119 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t120)
            continue
        } else {
            break Loop_loop115
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop98:
    for {
        var t106 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
        var t107 bool = t106 < 4
        var jp100 bool
        if t107 {
            var t110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t111 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t110, 1)
            var jp109 bool
            if t111 {
                jp109 = true
            } else {
                var t112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
                var t113 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t112, 3)
                var t114 bool = !t113
                jp109 = t114
            }
            jp100 = jp109
        } else {
            jp100 = false
        }
        if jp100 {
            var t101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
            var t102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t103 int = t101 + t102
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__2, t103)
            var t104 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t105 int = t104 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__1, t105)
            continue
        } else {
            break Loop_loop98
        }
    }
    var t83 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t83)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop86:
    for {
        var mtmp75 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
        var jp88 bool
        switch mtmp75 {
        case 0:
            jp88 = true
        case 1:
            var t96 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t97 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t96, 0)
            var jp95 bool
            if t97 {
                jp95 = true
            } else {
                jp95 = false
            }
            jp88 = jp95
        case 2:
            jp88 = true
        default:
            jp88 = false
        }
        if jp88 {
            var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t90 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t91 int = t89 + t90
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__4, t91)
            var t92 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t93 int = t92 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(k__3, t93)
            continue
        } else {
            break Loop_loop86
        }
    }
    var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t85)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv124 *ref_int_x
    var t125 *ref_int_x = ref__Ref_3int(value__207)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv127 int
    var t128 int = ref_get__Ref_3int(self__208)
    retv127 = t128
    return retv127
}

func println__T_int(value__1 int) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv135 bool
    var t136 bool = self__59 == other__60
    retv135 = t136
    return retv135
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv138 string
    var t139 string = _goml_runtime_core_int_to_string(self__40)
    retv138 = t139
    return retv138
}

func main() {
    main0()
}
