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
    var t185 int32
    var inline288 int32 = ref_get__Ref_5int32(cell__0)
    t185 = inline288
    var t186 int32 = t185 + 1
    ref_set__Ref_5int32(cell__0, t186)
    var inline284 int32 = ref_get__Ref_5int32(cell__0)
    return inline284
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline294 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline294
    var t190 bool = !current__2
    ref_set__Ref_4bool(flag__1, t190)
    var inline290 bool = ref_get__Ref_4bool(flag__1)
    return inline290
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline302 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline302
    var before__5 int32
    var inline300 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline300
    var t194 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t194)
    var t195 int32
    var inline296 int32 = ref_get__Ref_5int32(inner__4)
    t195 = inline296
    var t196 int32 = before__5 + t195
    return t196
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t216 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t216)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t217 int32
    var inline380 int32 = ref_get__Ref_5int32(inner__18)
    t217 = inline380
    var t218 int32 = t217 + bumped__15
    ref_set__Ref_5int32(inner__18, t218)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline373 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline374 int32 = inline373 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline374)
    var inline376 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline376
    var pair_total__21 int32
    var inline362 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline363 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline364 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline362)
    var inline365 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline363)
    var inline366 int32 = inline364 + inline365
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline362, inline366)
    var inline368 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline362)
    var inline369 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline363)
    var inline370 int32 = inline368 + inline369
    pair_total__21 = inline370
    var reassigned__22 int32
    var inline356 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline357 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline356)
    var inline358 int32 = inline357 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline356, inline358)
    var inline360 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline356)
    reassigned__22 = inline360
    var bool_check__23 bool = !false
    var t219 int32
    var inline354 int32 = ref_get__Ref_5int32(counter__12)
    t219 = inline354
    var t220 int32 = bumped__15 + t219
    var t221 string
    var inline352 string = _goml_runtime_core_int32_to_string(t220)
    t221 = inline352
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline349)
    var t222 int32 = nested_total_val__19 + alias_total__20
    var t223 int32 = t222 + reassigned__22
    var t224 string
    var inline347 string = _goml_runtime_core_int32_to_string(t223)
    t224 = inline347
    var inline344 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline344)
    var t225 string
    var inline342 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t225 = inline342
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline339)
    var jp230 bool
    if flipped__16 {
        jp230 = flipped_again__17
    } else {
        jp230 = false
    }
    var jp227 bool
    if jp230 {
        jp227 = bool_check__23
    } else {
        jp227 = false
    }
    var t228 string
    var inline337 string = _goml_runtime_core_bool_to_string(jp227)
    t228 = inline337
    var inline334 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline334)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t233 int32 = ref_get__Ref_5int32(self__258)
    return t233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__258 *ref_Ref_5int32_x) *ref_int32_x {
    var t243 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__258)
    return t243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t246 *ref_int32_x = ref__Ref_5int32(value__257)
    return t246
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__257 bool) *ref_bool_x {
    var t249 *ref_bool_x = ref__Ref_4bool(value__257)
    return t249
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__257 *ref_int32_x) *ref_Ref_5int32_x {
    var t252 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__257)
    return t252
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
