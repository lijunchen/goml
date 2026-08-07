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
    var t149 int32
    var inline252 int32 = ref_get__Ref_5int32(cell__0)
    t149 = inline252
    var t150 int32 = t149 + 1
    ref_set__Ref_5int32(cell__0, t150)
    var inline248 int32 = ref_get__Ref_5int32(cell__0)
    return inline248
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline258 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline258
    var t154 bool = !current__2
    ref_set__Ref_4bool(flag__1, t154)
    var inline254 bool = ref_get__Ref_4bool(flag__1)
    return inline254
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline266 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline266
    var before__5 int32
    var inline264 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline264
    var t158 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t158)
    var t159 int32
    var inline260 int32 = ref_get__Ref_5int32(inner__4)
    t159 = inline260
    var t160 int32 = before__5 + t159
    return t160
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t180 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t180)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t181 int32
    var inline344 int32 = ref_get__Ref_5int32(inner__18)
    t181 = inline344
    var t182 int32 = t181 + bumped__15
    ref_set__Ref_5int32(inner__18, t182)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline337 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline338 int32 = inline337 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline338)
    var inline340 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline340
    var pair_total__21 int32
    var inline326 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline327 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline328 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline326)
    var inline329 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline327)
    var inline330 int32 = inline328 + inline329
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline326, inline330)
    var inline332 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline326)
    var inline333 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline327)
    var inline334 int32 = inline332 + inline333
    pair_total__21 = inline334
    var reassigned__22 int32
    var inline320 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline321 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline320)
    var inline322 int32 = inline321 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline320, inline322)
    var inline324 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline320)
    reassigned__22 = inline324
    var bool_check__23 bool = !false
    var t183 int32
    var inline318 int32 = ref_get__Ref_5int32(counter__12)
    t183 = inline318
    var t184 int32 = bumped__15 + t183
    var t185 string
    var inline316 string = _goml_runtime_core_int32_to_string(t184)
    t185 = inline316
    var inline313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline313)
    var t186 int32 = nested_total_val__19 + alias_total__20
    var t187 int32 = t186 + reassigned__22
    var t188 string
    var inline311 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline311
    var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline308)
    var t189 string
    var inline306 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t189 = inline306
    var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline303)
    var jp194 bool
    if flipped__16 {
        jp194 = flipped_again__17
    } else {
        jp194 = false
    }
    var jp191 bool
    if jp194 {
        jp191 = bool_check__23
    } else {
        jp191 = false
    }
    var t192 string
    var inline301 string = _goml_runtime_core_bool_to_string(jp191)
    t192 = inline301
    var inline298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline298)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__233 *ref_int32_x) int32 {
    var t197 int32 = ref_get__Ref_5int32(self__233)
    return t197
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__234 *ref_int32_x, value__235 int32) struct{} {
    ref_set__Ref_5int32(self__234, value__235)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__233 *ref_Ref_5int32_x) *ref_int32_x {
    var t207 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__233)
    return t207
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__232 int32) *ref_int32_x {
    var t210 *ref_int32_x = ref__Ref_5int32(value__232)
    return t210
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__232 bool) *ref_bool_x {
    var t213 *ref_bool_x = ref__Ref_4bool(value__232)
    return t213
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__232 *ref_int32_x) *ref_Ref_5int32_x {
    var t216 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__232)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
