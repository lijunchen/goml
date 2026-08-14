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
    var t200 int32
    var inline303 int32 = ref_get__Ref_5int32(cell__0)
    t200 = inline303
    var t201 int32 = t200 + 1
    ref_set__Ref_5int32(cell__0, t201)
    var inline299 int32 = ref_get__Ref_5int32(cell__0)
    return inline299
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline309 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline309
    var t205 bool = !current__2
    ref_set__Ref_4bool(flag__1, t205)
    var inline305 bool = ref_get__Ref_4bool(flag__1)
    return inline305
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline317 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline317
    var before__5 int32
    var inline315 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline315
    var t209 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t209)
    var t210 int32
    var inline311 int32 = ref_get__Ref_5int32(inner__4)
    t210 = inline311
    var t211 int32 = before__5 + t210
    return t211
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t231 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t231)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t232 int32
    var inline395 int32 = ref_get__Ref_5int32(inner__18)
    t232 = inline395
    var t233 int32 = t232 + bumped__15
    ref_set__Ref_5int32(inner__18, t233)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline388 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline389 int32 = inline388 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline389)
    var inline391 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline391
    var pair_total__21 int32
    var inline377 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline378 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline377)
    var inline380 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline378)
    var inline381 int32 = inline379 + inline380
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline377, inline381)
    var inline383 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline377)
    var inline384 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline378)
    var inline385 int32 = inline383 + inline384
    pair_total__21 = inline385
    var reassigned__22 int32
    var inline371 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline372 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline371)
    var inline373 int32 = inline372 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline371, inline373)
    var inline375 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline371)
    reassigned__22 = inline375
    var bool_check__23 bool = !false
    var t234 int32
    var inline369 int32 = ref_get__Ref_5int32(counter__12)
    t234 = inline369
    var t235 int32 = bumped__15 + t234
    var t236 string
    var inline367 string = _goml_runtime_core_int32_to_string(t235)
    t236 = inline367
    var inline364 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline364)
    var t237 int32 = nested_total_val__19 + alias_total__20
    var t238 int32 = t237 + reassigned__22
    var t239 string
    var inline362 string = _goml_runtime_core_int32_to_string(t238)
    t239 = inline362
    var inline359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline359)
    var t240 string
    var inline357 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t240 = inline357
    var inline354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline354)
    var jp245 bool
    if flipped__16 {
        jp245 = flipped_again__17
    } else {
        jp245 = false
    }
    var jp242 bool
    if jp245 {
        jp242 = bool_check__23
    } else {
        jp242 = false
    }
    var t243 string
    var inline352 string = _goml_runtime_core_bool_to_string(jp242)
    t243 = inline352
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline349)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t248 int32 = ref_get__Ref_5int32(self__274)
    return t248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__275 *ref_int32_x, value__276 int32) struct{} {
    ref_set__Ref_5int32(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__274 *ref_Ref_5int32_x) *ref_int32_x {
    var t258 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__274)
    return t258
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t261 *ref_int32_x = ref__Ref_5int32(value__273)
    return t261
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__273 bool) *ref_bool_x {
    var t264 *ref_bool_x = ref__Ref_4bool(value__273)
    return t264
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__273 *ref_int32_x) *ref_Ref_5int32_x {
    var t267 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__273)
    return t267
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
