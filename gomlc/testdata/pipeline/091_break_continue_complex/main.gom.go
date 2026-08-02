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
    Loop_loop182:
    for {
        var t183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t184 bool = t183 <= 100
        if t184 {
            var t191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t192 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t191, 50)
            if t192 {
                break Loop_loop182
            } else {
                var t186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
                var t187 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t188 int = t186 + t187
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__0, t188)
                var t189 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
                var t190 int = t189 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t190)
                continue
            }
        } else {
            break Loop_loop182
        }
    }
    print__T_string("sum up to break: ")
    var t169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__0)
    println__T_int(t169)
    var even_sum__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop172:
    for {
        var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
        var t174 bool = t173 <= 10
        if t174 {
            var cur__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t175 int = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t175)
            var t177 int = cur__4 / 2
            var t178 int = t177 * 2
            var t179 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(cur__4, t178)
            if t179 {
                var t180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
                var t181 int = t180 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(even_sum__2, t181)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop172
        }
    }
    print__T_string("even sum: ")
    var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(even_sum__2)
    println__T_int(t171)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv194 *ref_int_x
    var t195 *ref_int_x = ref__Ref_3int(value__207)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv197 int
    var t198 int = ref_get__Ref_3int(self__208)
    retv197 = t198
    return retv197
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv200 bool
    var t201 bool = self__59 == other__60
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t205)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t208 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv211 string
    retv211 = self__38
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv213 string
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    retv213 = t214
    return retv213
}

func main() {
    main0()
}
