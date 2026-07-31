package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

func sum_to(limit__0 int32) int32 {
    var retv163 int32
    var acc__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop166:
    for {
        var t167 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var t168 bool = t167 < limit__0
        if t168 {
            var current__3 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
            var t169 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
            var t170 int32 = t169 + current__3
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__1, t170)
            var t171 int32 = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t171)
            continue
        } else {
            break Loop_loop166
        }
    }
    var t165 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
    retv163 = t165
    return retv163
}

func sum_even(limit__4 int32) int32 {
    var retv173 int32
    var acc__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__6 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var is_even__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop176:
    for {
        var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
        var t178 bool = t177 < limit__4
        if t178 {
            var current__8 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
            var t179 int32 = current__8 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__6, t179)
            var add_now__9 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(is_even__7)
            var t180 bool = !add_now__9
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(is_even__7, t180)
            if add_now__9 {
                var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
                var t183 int32 = t182 + current__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__5, t183)
            } else {}
            continue
        } else {
            break Loop_loop176
        }
    }
    var t175 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
    retv173 = t175
    return retv173
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    print__T_string("sum_to(5)=")
    println__T_int32(first__10)
    print__T_string("sum_even(6)=")
    println__T_int32(evens__11)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv187 *ref_int32_x
    var t188 *ref_int32_x = ref__Ref_5int32(value__207)
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv190 int32
    var t191 int32 = ref_get__Ref_5int32(self__208)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv195 *ref_bool_x
    var t196 *ref_bool_x = ref__Ref_4bool(value__207)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv198 bool
    var t199 bool = ref_get__Ref_4bool(self__208)
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t203)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t206 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv209 string
    retv209 = self__38
    return retv209
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv211 string
    var t212 string = _goml_runtime_core_int32_to_string(self__43)
    retv211 = t212
    return retv211
}

func main() {
    main0()
}
