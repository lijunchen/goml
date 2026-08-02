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
    var t168 int32
    var inline271 int32 = ref_get__Ref_5int32(cell__0)
    t168 = inline271
    var t169 int32 = t168 + 1
    ref_set__Ref_5int32(cell__0, t169)
    var inline267 int32 = ref_get__Ref_5int32(cell__0)
    return inline267
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline277 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline277
    var t173 bool = !current__2
    ref_set__Ref_4bool(flag__1, t173)
    var inline273 bool = ref_get__Ref_4bool(flag__1)
    return inline273
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline285 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline285
    var before__5 int32
    var inline283 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline283
    var t177 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t177)
    var t178 int32
    var inline279 int32 = ref_get__Ref_5int32(inner__4)
    t178 = inline279
    var t179 int32 = before__5 + t178
    return t179
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t199 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t199)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t200 int32
    var inline363 int32 = ref_get__Ref_5int32(inner__18)
    t200 = inline363
    var t201 int32 = t200 + bumped__15
    ref_set__Ref_5int32(inner__18, t201)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline356 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline357 int32 = inline356 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline357)
    var inline359 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline359
    var pair_total__21 int32
    var inline345 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline346 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline347 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline345)
    var inline348 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline346)
    var inline349 int32 = inline347 + inline348
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline345, inline349)
    var inline351 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline345)
    var inline352 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline346)
    var inline353 int32 = inline351 + inline352
    pair_total__21 = inline353
    var reassigned__22 int32
    var inline339 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline340 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline339)
    var inline341 int32 = inline340 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline339, inline341)
    var inline343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline339)
    reassigned__22 = inline343
    var bool_check__23 bool = !false
    var t202 int32
    var inline337 int32 = ref_get__Ref_5int32(counter__12)
    t202 = inline337
    var t203 int32 = bumped__15 + t202
    var t204 string
    var inline335 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline335
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline332)
    var t205 int32 = nested_total_val__19 + alias_total__20
    var t206 int32 = t205 + reassigned__22
    var t207 string
    var inline330 string = _goml_runtime_core_int32_to_string(t206)
    t207 = inline330
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline327)
    var t208 string
    var inline325 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t208 = inline325
    var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline322)
    var jp213 bool
    if flipped__16 {
        jp213 = flipped_again__17
    } else {
        jp213 = false
    }
    var jp210 bool
    if jp213 {
        jp210 = bool_check__23
    } else {
        jp210 = false
    }
    var t211 string
    var inline320 string = _goml_runtime_core_bool_to_string(jp210)
    t211 = inline320
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline317)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t216 int32 = ref_get__Ref_5int32(self__208)
    return t216
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__208 *ref_Ref_5int32_x) *ref_int32_x {
    var t226 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__208)
    return t226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t229 *ref_int32_x = ref__Ref_5int32(value__207)
    return t229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var t232 *ref_bool_x = ref__Ref_4bool(value__207)
    return t232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__207 *ref_int32_x) *ref_Ref_5int32_x {
    var t235 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__207)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
