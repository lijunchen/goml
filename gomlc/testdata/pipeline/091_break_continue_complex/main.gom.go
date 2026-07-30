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
    Loop_loop95:
    for {
        var t96 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t97 bool = t96 <= 100
        if t97 {
            var t104 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t105 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t104, 50)
            if t105 {
                break Loop_loop95
            } else {
                var t99 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t101 int = t99 + t100
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t101)
                var t102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t103 int = t102 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t103)
                continue
            }
        } else {
            break Loop_loop95
        }
    }
    print__T_string("sum up to break: ")
    var t82 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t82)
    var even_sum__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop85:
    for {
        var t86 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t87 bool = t86 <= 10
        if t87 {
            var cur__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t88 int = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t88)
            var t90 int = cur__4 / 2
            var t91 int = t90 * 2
            var t92 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(cur__4, t91)
            if t92 {
                var t93 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
                var t94 int = t93 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(even_sum__2, t94)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop85
        }
    }
    print__T_string("even sum: ")
    var t84 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
    println__T_int(t84)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv107 *ref_int_x
    var t108 *ref_int_x = ref__Ref_3int(value__207)
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv110 int
    var t111 int = ref_get__Ref_3int(self__208)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv113 bool
    var t114 bool = self__59 == other__60
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t118)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int_to_string(self__40)
    retv126 = t127
    return retv126
}

func main() {
    main0()
}
