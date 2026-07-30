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
    Loop_loop135:
    for {
        var t136 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t137 bool = t136 <= 100
        if t137 {
            var t144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t145 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t144, 50)
            if t145 {
                break Loop_loop135
            } else {
                var t139 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t140 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t141 int = t139 + t140
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t141)
                var t142 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t143 int = t142 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t143)
                continue
            }
        } else {
            break Loop_loop135
        }
    }
    print__T_string("sum up to break: ")
    var t122 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t122)
    var even_sum__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop125:
    for {
        var t126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t127 bool = t126 <= 10
        if t127 {
            var cur__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t128 int = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t128)
            var t130 int = cur__4 / 2
            var t131 int = t130 * 2
            var t132 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(cur__4, t131)
            if t132 {
                var t133 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
                var t134 int = t133 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(even_sum__2, t134)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop125
        }
    }
    print__T_string("even sum: ")
    var t124 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
    println__T_int(t124)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv147 *ref_int_x
    var t148 *ref_int_x = ref__Ref_3int(value__207)
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv150 int
    var t151 int = ref_get__Ref_3int(self__208)
    retv150 = t151
    return retv150
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv153 bool
    var t154 bool = self__59 == other__60
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t158)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t161 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv164 string
    retv164 = self__38
    return retv164
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int_to_string(self__40)
    retv166 = t167
    return retv166
}

func main() {
    main0()
}
