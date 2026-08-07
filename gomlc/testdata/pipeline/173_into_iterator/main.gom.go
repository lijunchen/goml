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
    var inline386 int32 = 0
    var inline387 *ref_int32_x = ref__Ref_5int32(inline386)
    builds__7 = inline387
    var conversions__8 *ref_int32_x
    var inline383 int32 = 0
    var inline384 *ref_int32_x = ref__Ref_5int32(inline383)
    conversions__8 = inline384
    var t206 Numbers
    var inline374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline375 int32 = inline374 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline375)
    var inline377 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline377, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline377, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline377, 3)
    var inline381 Numbers = Numbers{
        values: inline377,
        conversions: conversions__8,
    }
    t206 = inline381
    var t207 int32 = sum__S_Numbers(t206)
    var inline371 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
    _goml_runtime_core_string_println(inline371)
    var t208 int32
    var inline369 int32 = ref_get__Ref_5int32(builds__7)
    t208 = inline369
    var inline366 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
    _goml_runtime_core_string_println(inline366)
    var t209 int32
    var inline364 int32 = ref_get__Ref_5int32(conversions__8)
    t209 = inline364
    var inline361 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
    _goml_runtime_core_string_println(inline361)
    var values__9 *_goml_vec_int32
    var inline359 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline359
    var inline356 int32 = 10
    vec_push__Vec_5int32(values__9, inline356)
    var inline353 int32 = 20
    vec_push__Vec_5int32(values__9, inline353)
    var inline350 int32 = 30
    vec_push__Vec_5int32(values__9, inline350)
    var t210 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline347 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
    _goml_runtime_core_string_println(inline347)
    var t211 []int32
    var inline343 int = 1
    var inline344 int = 3
    var inline345 []int32 = values__9.items[inline343:inline344]
    t211 = inline345
    var t212 int32 = _goml_m_sum____S__Slice_l_int32_r_(t211)
    var inline340 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
    _goml_runtime_core_string_println(inline340)
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
    var inline389 int = vec_len__Vec_5int32(self__228)
    len__230 = inline389
    var t220 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__229,
        len_1: len__230,
        self_2: self__228,
    }
    var t221 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t220)
    })
    return t221
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t224 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t224
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__176 *_goml_vec_int32, elem__177 int32) struct{} {
    vec_push__Vec_5int32(self__176, elem__177)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline410 int32 = 0
    var inline411 *ref_int32_x = ref__Ref_5int32(inline410)
    total__5 = inline411
    var for_iter177 FnIterator__int32
    var inline402 *ref_int32_x = source__4.conversions
    var inline403 *ref_int32_x = source__4.conversions
    var inline404 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline403)
    var inline405 int32 = inline404 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline402, inline405)
    var inline407 *_goml_vec_int32 = source__4.values
    var inline408 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline407)
    for_iter177 = inline408
    Loop_loop237:
    for {
        var for_next178 Option__int32
        var inline397 func() Option__int32 = for_iter177.next_fn
        var inline398 Option__int32 = inline397()
        for_next178 = inline398
        switch for_next178.(type) {
        case None:
            break Loop_loop237
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t239 int32
            var inline395 int32 = ref_get__Ref_5int32(total__5)
            t239 = inline395
            var t240 int32 = t239 + x179
            ref_set__Ref_5int32(total__5, t240)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline400 int32 = ref_get__Ref_5int32(total__5)
    return inline400
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline424 int32 = 0
    var inline425 *ref_int32_x = ref__Ref_5int32(inline424)
    total__5 = inline425
    var for_iter177 FnIterator__int32
    var inline422 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter177 = inline422
    Loop_loop245:
    for {
        var for_next178 Option__int32
        var inline417 func() Option__int32 = for_iter177.next_fn
        var inline418 Option__int32 = inline417()
        for_next178 = inline418
        switch for_next178.(type) {
        case None:
            break Loop_loop245
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t247 int32
            var inline415 int32 = ref_get__Ref_5int32(total__5)
            t247 = inline415
            var t248 int32 = t247 + x179
            ref_set__Ref_5int32(total__5, t248)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline420 int32 = ref_get__Ref_5int32(total__5)
    return inline420
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline438 int32 = 0
    var inline439 *ref_int32_x = ref__Ref_5int32(inline438)
    total__5 = inline439
    var for_iter177 FnIterator__int32
    var inline436 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter177 = inline436
    Loop_loop253:
    for {
        var for_next178 Option__int32
        var inline431 func() Option__int32 = for_iter177.next_fn
        var inline432 Option__int32 = inline431()
        for_next178 = inline432
        switch for_next178.(type) {
        case None:
            break Loop_loop253
        case Some:
            var x179 int32 = for_next178.(Some)._0
            var t255 int32
            var inline429 int32 = ref_get__Ref_5int32(total__5)
            t255 = inline429
            var t256 int32 = t255 + x179
            ref_set__Ref_5int32(total__5, t256)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline434 int32 = ref_get__Ref_5int32(total__5)
    return inline434
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t265 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t265
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t268 string = _goml_runtime_core_int32_to_string(self__72)
    return t268
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__240 []int32) FnIterator__int32 {
    var index__241 *ref_int_x = ref__Ref_3int(0)
    var len__242 int
    var inline451 int = len(self__240)
    len__242 = inline451
    var t281 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__241,
        len_1: len__242,
        self_2: self__240,
    }
    var t282 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t281)
    })
    return t282
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env190 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__229 *ref_int_x = env190.index_0
    var len__230 int = env190.len_1
    var self__228 *_goml_vec_int32 = env190.self_2
    var current__231 int = ref_get__Ref_3int(index__229)
    var t305 bool = current__231 < len__230
    if t305 {
        var value__232 int32 = vec_get__Vec_5int32(self__228, current__231)
        var t306 int = current__231 + 1
        ref_set__Ref_3int(index__229, t306)
        var t307 Option__int32 = Some{
            _0: value__232,
        }
        return t307
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env191 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__241 *ref_int_x = env191.index_0
    var len__242 int = env191.len_1
    var self__240 []int32 = env191.self_2
    var current__243 int = ref_get__Ref_3int(index__241)
    var t312 bool = current__243 < len__242
    if t312 {
        var value__244 int32
        var inline453 int32 = self__240[current__243]
        value__244 = inline453
        var t313 int = current__243 + 1
        ref_set__Ref_3int(index__241, t313)
        var t314 Option__int32 = Some{
            _0: value__244,
        }
        return t314
    } else {
        return None{}
    }
}

func main() {
    main0()
}
