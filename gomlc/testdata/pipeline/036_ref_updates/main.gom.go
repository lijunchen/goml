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
    var t190 int32
    var inline293 int32 = ref_get__Ref_5int32(cell__0)
    t190 = inline293
    var t191 int32 = t190 + 1
    ref_set__Ref_5int32(cell__0, t191)
    var inline289 int32 = ref_get__Ref_5int32(cell__0)
    return inline289
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline299 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline299
    var t195 bool = !current__2
    ref_set__Ref_4bool(flag__1, t195)
    var inline295 bool = ref_get__Ref_4bool(flag__1)
    return inline295
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline307 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline307
    var before__5 int32
    var inline305 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline305
    var t199 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t199)
    var t200 int32
    var inline301 int32 = ref_get__Ref_5int32(inner__4)
    t200 = inline301
    var t201 int32 = before__5 + t200
    return t201
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t221 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t221)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t222 int32
    var inline385 int32 = ref_get__Ref_5int32(inner__18)
    t222 = inline385
    var t223 int32 = t222 + bumped__15
    ref_set__Ref_5int32(inner__18, t223)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline378 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline379 int32 = inline378 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline379)
    var inline381 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline381
    var pair_total__21 int32
    var inline367 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline368 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline369 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline367)
    var inline370 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline368)
    var inline371 int32 = inline369 + inline370
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline367, inline371)
    var inline373 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline367)
    var inline374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline368)
    var inline375 int32 = inline373 + inline374
    pair_total__21 = inline375
    var reassigned__22 int32
    var inline361 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline362 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline361)
    var inline363 int32 = inline362 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline361, inline363)
    var inline365 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline361)
    reassigned__22 = inline365
    var bool_check__23 bool = !false
    var t224 int32
    var inline359 int32 = ref_get__Ref_5int32(counter__12)
    t224 = inline359
    var t225 int32 = bumped__15 + t224
    var t226 string
    var inline357 string = _goml_runtime_core_int32_to_string(t225)
    t226 = inline357
    var inline354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline354)
    var t227 int32 = nested_total_val__19 + alias_total__20
    var t228 int32 = t227 + reassigned__22
    var t229 string
    var inline352 string = _goml_runtime_core_int32_to_string(t228)
    t229 = inline352
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline349)
    var t230 string
    var inline347 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t230 = inline347
    var inline344 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline344)
    var jp235 bool
    if flipped__16 {
        jp235 = flipped_again__17
    } else {
        jp235 = false
    }
    var jp232 bool
    if jp235 {
        jp232 = bool_check__23
    } else {
        jp232 = false
    }
    var t233 string
    var inline342 string = _goml_runtime_core_bool_to_string(jp232)
    t233 = inline342
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline339)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t238 int32 = ref_get__Ref_5int32(self__237)
    return t238
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__238 *ref_int32_x, value__239 int32) struct{} {
    ref_set__Ref_5int32(self__238, value__239)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__237 *ref_Ref_5int32_x) *ref_int32_x {
    var t248 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__237)
    return t248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t251 *ref_int32_x = ref__Ref_5int32(value__236)
    return t251
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__236 bool) *ref_bool_x {
    var t254 *ref_bool_x = ref__Ref_4bool(value__236)
    return t254
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__236 *ref_int32_x) *ref_Ref_5int32_x {
    var t257 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__236)
    return t257
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
