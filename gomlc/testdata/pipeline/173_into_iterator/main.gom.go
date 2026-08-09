package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

type Numbers struct {
    values *_goml_vec_int32
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 []int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    var builds__7 *ref_int32_x
    var inline389 int32 = 0
    var inline390 *ref_int32_x = ref__Ref_5int32(inline389)
    builds__7 = inline390
    var conversions__8 *ref_int32_x
    var inline386 int32 = 0
    var inline387 *ref_int32_x = ref__Ref_5int32(inline386)
    conversions__8 = inline387
    var t206 Numbers
    var inline377 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline378 int32 = inline377 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline378)
    var inline380 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline380, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline380, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline380, 3)
    var inline384 Numbers = Numbers{
        values: inline380,
        conversions: conversions__8,
    }
    t206 = inline384
    var t207 int32 = sum__S_Numbers(t206)
    var inline374 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
    _goml_runtime_core_string_println(inline374)
    var t208 int32
    var inline372 int32 = ref_get__Ref_5int32(builds__7)
    t208 = inline372
    var inline369 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
    _goml_runtime_core_string_println(inline369)
    var t209 int32
    var inline367 int32 = ref_get__Ref_5int32(conversions__8)
    t209 = inline367
    var inline364 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
    _goml_runtime_core_string_println(inline364)
    var values__9 *_goml_vec_int32
    var inline362 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline362
    var inline359 int32 = 10
    vec_push__Vec_5int32(values__9, inline359)
    var inline356 int32 = 20
    vec_push__Vec_5int32(values__9, inline356)
    var inline353 int32 = 30
    vec_push__Vec_5int32(values__9, inline353)
    var t210 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline350 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
    _goml_runtime_core_string_println(inline350)
    var t211 []int32
    var inline346 int = 1
    var inline347 int = 3
    var inline348 []int32 = values__9.items[inline346:inline347]
    t211 = inline348
    var t212 int32 = _goml_m_sum____S__Slice_l_int32_r_(t211)
    var inline343 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
    _goml_runtime_core_string_println(inline343)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t215 int32 = ref_get__Ref_5int32(self__258)
    return t215
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__228 *_goml_vec_int32) FnIterator__int32 {
    var index__229 *ref_int_x = ref__Ref_3int(0)
    var len__230 int
    var inline394 int = vec_len__Vec_5int32(self__228)
    len__230 = inline394
    var t220 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__229,
        len_1: len__230,
        self_2: self__228,
    }
    var t221 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t220)
    }
    var inline392 FnIterator__int32 = FnIterator__int32{
        next_fn: t221,
    }
    return inline392
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t225 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t225
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__176 *_goml_vec_int32, elem__177 int32) struct{} {
    vec_push__Vec_5int32(self__176, elem__177)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline415 int32 = 0
    var inline416 *ref_int32_x = ref__Ref_5int32(inline415)
    total__5 = inline416
    var for_iter177 FnIterator__int32
    var inline407 *ref_int32_x = source__4.conversions
    var inline408 *ref_int32_x = source__4.conversions
    var inline409 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline408)
    var inline410 int32 = inline409 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline407, inline410)
    var inline412 *_goml_vec_int32 = source__4.values
    var inline413 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline412)
    for_iter177 = inline413
    Loop_loop238:
    for {
        var for_next178 Option__int32
        var inline402 func() Option__int32 = for_iter177.next_fn
        var inline403 Option__int32 = inline402()
        for_next178 = inline403
        switch for_next178.(type) {
        case None:
            break Loop_loop238
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t240 int32
            var inline400 int32 = ref_get__Ref_5int32(total__5)
            t240 = inline400
            var t241 int32 = t240 + x179
            ref_set__Ref_5int32(total__5, t241)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline405 int32 = ref_get__Ref_5int32(total__5)
    return inline405
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline429 int32 = 0
    var inline430 *ref_int32_x = ref__Ref_5int32(inline429)
    total__5 = inline430
    var for_iter177 FnIterator__int32
    var inline427 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter177 = inline427
    Loop_loop246:
    for {
        var for_next178 Option__int32
        var inline422 func() Option__int32 = for_iter177.next_fn
        var inline423 Option__int32 = inline422()
        for_next178 = inline423
        switch for_next178.(type) {
        case None:
            break Loop_loop246
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t248 int32
            var inline420 int32 = ref_get__Ref_5int32(total__5)
            t248 = inline420
            var t249 int32 = t248 + x179
            ref_set__Ref_5int32(total__5, t249)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline425 int32 = ref_get__Ref_5int32(total__5)
    return inline425
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline443 int32 = 0
    var inline444 *ref_int32_x = ref__Ref_5int32(inline443)
    total__5 = inline444
    var for_iter177 FnIterator__int32
    var inline441 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter177 = inline441
    Loop_loop254:
    for {
        var for_next178 Option__int32
        var inline436 func() Option__int32 = for_iter177.next_fn
        var inline437 Option__int32 = inline436()
        for_next178 = inline437
        switch for_next178.(type) {
        case None:
            break Loop_loop254
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t256 int32
            var inline434 int32 = ref_get__Ref_5int32(total__5)
            t256 = inline434
            var t257 int32 = t256 + x179
            ref_set__Ref_5int32(total__5, t257)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline439 int32 = ref_get__Ref_5int32(total__5)
    return inline439
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t269 string = _goml_runtime_core_int32_to_string(self__72)
    return t269
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__240 []int32) FnIterator__int32 {
    var index__241 *ref_int_x = ref__Ref_3int(0)
    var len__242 int
    var inline460 int = len(self__240)
    len__242 = inline460
    var t282 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__241,
        len_1: len__242,
        self_2: self__240,
    }
    var t283 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t282)
    }
    var inline458 FnIterator__int32 = FnIterator__int32{
        next_fn: t283,
    }
    return inline458
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env190 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__229 *ref_int_x = env190.index_0
    var len__230 int = env190.len_1
    var self__228 *_goml_vec_int32 = env190.self_2
    var current__231 int = ref_get__Ref_3int(index__229)
    var t307 bool = current__231 < len__230
    if t307 {
        var value__232 int32 = vec_get__Vec_5int32(self__228, current__231)
        var t308 int = current__231 + 1
        ref_set__Ref_3int(index__229, t308)
        var t309 Option__int32 = Some{
            _0: value__232,
        }
        return t309
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env191 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__241 *ref_int_x = env191.index_0
    var len__242 int = env191.len_1
    var self__240 []int32 = env191.self_2
    var current__243 int = ref_get__Ref_3int(index__241)
    var t314 bool = current__243 < len__242
    if t314 {
        var value__244 int32
        var inline462 int32 = self__240[current__243]
        value__244 = inline462
        var t315 int = current__243 + 1
        ref_set__Ref_3int(index__241, t315)
        var t316 Option__int32 = Some{
            _0: value__244,
        }
        return t316
    } else {
        return None{}
    }
}

func main() {
    main0()
}
