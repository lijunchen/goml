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
    var retv159 int32
    var jp161 int32
    if flag__0 {
        retv159 = 10
        return retv159
    } else {
        jp161 = 20
        var value__1 int32 = jp161
        var t162 int32 = value__1 + 1
        retv159 = t162
        return retv159
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop165:
    for {
        var t166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
        var t167 bool = t166 < 2
        if t167 {
            var t168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t169 int = t168 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(count__2, t169)
            var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t174 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t173, 1)
            var jp171 int
            if t174 {
                continue
            } else {
                jp171 = 7
                var value__3 int = jp171
                println__T_int(value__3)
                continue
            }
        } else {
            break Loop_loop165
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp179 int
        if stop__4 {
            return struct{}{}
        } else {
            jp179 = 9
            var value__5 int = jp179
            println__T_int(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t181 int32 = choose(false)
    println__T_int32(t181)
    var t182 int32 = choose(true)
    println__T_int32(t182)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv185 *ref_int_x
    var t186 *ref_int_x = ref__Ref_3int(value__207)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv188 int
    var t189 int = ref_get__Ref_3int(self__208)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv193 bool
    var t194 bool = self__59 == other__60
    retv193 = t194
    return retv193
}

func println__T_int(value__1 int) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t199 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv202 string
    var t203 string = _goml_runtime_core_int_to_string(self__40)
    retv202 = t203
    return retv202
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv205 string
    var t206 string = _goml_runtime_core_int32_to_string(self__43)
    retv205 = t206
    return retv205
}

func main() {
    main0()
}
