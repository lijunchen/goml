package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
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

func choose(flag__0 bool) int32 {
    var retv162 int32
    var jp164 int32
    if flag__0 {
        retv162 = 10
        return retv162
    } else {
        jp164 = 20
        var value__1 int32 = jp164
        var t165 int32 = value__1 + 1
        retv162 = t165
        return retv162
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop168:
    for {
        var t169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
        var t170 bool = t169 < 2
        if t170 {
            var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t172 int = t171 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(count__2, t172)
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t177 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t176, 1)
            var jp174 int
            if t177 {
                continue
            } else {
                jp174 = 7
                var value__3 int = jp174
                println__T_int(value__3)
                continue
            }
        } else {
            break Loop_loop168
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp182 int
        if stop__4 {
            return struct{}{}
        } else {
            jp182 = 9
            var value__5 int = jp182
            println__T_int(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t184 int32 = choose(false)
    println__T_int32(t184)
    var t185 int32 = choose(true)
    println__T_int32(t185)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv188 *ref_int_x
    var t189 *ref_int_x = ref__Ref_3int(value__207)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv191 int
    var t192 int = ref_get__Ref_3int(self__208)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv196 bool
    var t197 bool = self__59 == other__60
    retv196 = t197
    return retv196
}

func println__T_int(value__1 int) struct{} {
    var t199 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t199)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv205 string
    var t206 string = _goml_runtime_core_int_to_string(self__40)
    retv205 = t206
    return retv205
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv208 string
    var t209 string = _goml_runtime_core_int32_to_string(self__43)
    retv208 = t209
    return retv208
}

func main() {
    main0()
}
