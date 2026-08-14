package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
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

type ref_Ref_5int32_x struct {
    value *ref_int32_x
}

func ref__Ref_10Ref_5int32(value *ref_int32_x) *ref_Ref_5int32_x {
    return &ref_Ref_5int32_x{
        value: value,
    }
}

func ref_get__Ref_10Ref_5int32(reference *ref_Ref_5int32_x) *ref_int32_x {
    return reference.value
}

func bump(cell__0 *ref_int32_x) int32 {
    var t195 int32
    var inline298 int32 = ref_get__Ref_5int32(cell__0)
    t195 = inline298
    var t196 int32 = t195 + 1
    ref_set__Ref_5int32(cell__0, t196)
    var inline294 int32 = ref_get__Ref_5int32(cell__0)
    return inline294
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline304 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline304
    var t200 bool = !current__2
    ref_set__Ref_4bool(flag__1, t200)
    var inline300 bool = ref_get__Ref_4bool(flag__1)
    return inline300
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline312 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline312
    var before__5 int32
    var inline310 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline310
    var t204 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t204)
    var t205 int32
    var inline306 int32 = ref_get__Ref_5int32(inner__4)
    t205 = inline306
    var t206 int32 = before__5 + t205
    return t206
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t226 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t226)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t227 int32
    var inline390 int32 = ref_get__Ref_5int32(inner__18)
    t227 = inline390
    var t228 int32 = t227 + bumped__15
    ref_set__Ref_5int32(inner__18, t228)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline383 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline384 int32 = inline383 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline384)
    var inline386 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline386
    var pair_total__21 int32
    var inline372 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline373 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline372)
    var inline375 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline373)
    var inline376 int32 = inline374 + inline375
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline372, inline376)
    var inline378 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline372)
    var inline379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline373)
    var inline380 int32 = inline378 + inline379
    pair_total__21 = inline380
    var reassigned__22 int32
    var inline366 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline367 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline366)
    var inline368 int32 = inline367 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline366, inline368)
    var inline370 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline366)
    reassigned__22 = inline370
    var bool_check__23 bool = !false
    var t229 int32
    var inline364 int32 = ref_get__Ref_5int32(counter__12)
    t229 = inline364
    var t230 int32 = bumped__15 + t229
    var t231 string
    var inline362 string = _goml_runtime_core_int32_to_string(t230)
    t231 = inline362
    var inline359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline359)
    var t232 int32 = nested_total_val__19 + alias_total__20
    var t233 int32 = t232 + reassigned__22
    var t234 string
    var inline357 string = _goml_runtime_core_int32_to_string(t233)
    t234 = inline357
    var inline354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline354)
    var t235 string
    var inline352 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t235 = inline352
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline349)
    var jp240 bool
    if flipped__16 {
        jp240 = flipped_again__17
    } else {
        jp240 = false
    }
    var jp237 bool
    if jp240 {
        jp237 = bool_check__23
    } else {
        jp237 = false
    }
    var t238 string
    var inline347 string = _goml_runtime_core_bool_to_string(jp237)
    t238 = inline347
    var inline344 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline344)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t243 int32 = ref_get__Ref_5int32(self__271)
    return t243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__271 *ref_Ref_5int32_x) *ref_int32_x {
    var t253 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__271)
    return t253
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t256 *ref_int32_x = ref__Ref_5int32(value__270)
    return t256
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__270 bool) *ref_bool_x {
    var t259 *ref_bool_x = ref__Ref_4bool(value__270)
    return t259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__270 *ref_int32_x) *ref_Ref_5int32_x {
    var t262 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__270)
    return t262
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
