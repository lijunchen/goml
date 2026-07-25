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
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop91:
    for {
        var t92 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t93 bool = t92 <= 100
        if t93 {
            var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t101 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t100, 50)
            if t101 {
                break Loop_loop91
            } else {
                var t95 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t96 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t97 int = t95 + t96
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t97)
                var t98 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t99 int = t98 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t99)
                continue
            }
        } else {
            break Loop_loop91
        }
    }
    print__T_string("sum up to break: ")
    var t78 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t78)
    var even_sum__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop81:
    for {
        var t82 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t83 bool = t82 <= 10
        if t83 {
            var cur__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t84 int = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t84)
            var t86 int = cur__4 / 2
            var t87 int = t86 * 2
            var t88 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(cur__4, t87)
            if t88 {
                var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
                var t90 int = t89 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(even_sum__2, t90)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop81
        }
    }
    print__T_string("even sum: ")
    var t80 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
    println__T_int(t80)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv103 *ref_int_x
    var t104 *ref_int_x = ref__Ref_3int(value__209)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv106 int
    var t107 int = ref_get__Ref_3int(self__210)
    retv106 = t107
    return retv106
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv109 bool
    var t110 bool = self__59 == other__60
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t114)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv120 string
    retv120 = self__38
    return retv120
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int_to_string(self__40)
    retv122 = t123
    return retv122
}

func main() {
    main0()
}
