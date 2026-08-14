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
    var inline404 int32 = 0
    var inline405 *ref_int32_x = ref__Ref_5int32(inline404)
    builds__7 = inline405
    var conversions__8 *ref_int32_x
    var inline401 int32 = 0
    var inline402 *ref_int32_x = ref__Ref_5int32(inline401)
    conversions__8 = inline402
    var t221 Numbers
    var inline392 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline393 int32 = inline392 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline393)
    var inline395 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline395, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline395, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline395, 3)
    var inline399 Numbers = Numbers{
        values: inline395,
        conversions: conversions__8,
    }
    t221 = inline399
    var t222 int32 = sum__S_Numbers(t221)
    var inline389 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t222)
    _goml_runtime_core_string_println(inline389)
    var t223 int32
    var inline387 int32 = ref_get__Ref_5int32(builds__7)
    t223 = inline387
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t223)
    _goml_runtime_core_string_println(inline384)
    var t224 int32
    var inline382 int32 = ref_get__Ref_5int32(conversions__8)
    t224 = inline382
    var inline379 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t224)
    _goml_runtime_core_string_println(inline379)
    var values__9 *_goml_vec_int32
    var inline377 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline377
    var inline374 int32 = 10
    vec_push__Vec_5int32(values__9, inline374)
    var inline371 int32 = 20
    vec_push__Vec_5int32(values__9, inline371)
    var inline368 int32 = 30
    vec_push__Vec_5int32(values__9, inline368)
    var t225 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline365 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t225)
    _goml_runtime_core_string_println(inline365)
    var t226 []int32
    var inline361 int = 1
    var inline362 int = 3
    var inline363 []int32 = values__9.items[inline361:inline362]
    t226 = inline363
    var t227 int32 = _goml_m_sum____S__Slice_l_int32_r_(t226)
    var inline358 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t227)
    _goml_runtime_core_string_println(inline358)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t230 int32 = ref_get__Ref_5int32(self__274)
    return t230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__275 *ref_int32_x, value__276 int32) struct{} {
    ref_set__Ref_5int32(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__230 *_goml_vec_int32) FnIterator__int32 {
    var index__231 *ref_int_x = ref__Ref_3int(0)
    var len__232 int
    var inline409 int = vec_len__Vec_5int32(self__230)
    len__232 = inline409
    var t235 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__231,
        len_1: len__232,
        self_2: self__230,
    }
    var t236 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t235)
    }
    var inline407 FnIterator__int32 = FnIterator__int32{
        next_fn: t236,
    }
    return inline407
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t240 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t240
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline430 int32 = 0
    var inline431 *ref_int32_x = ref__Ref_5int32(inline430)
    total__5 = inline431
    var for_iter192 FnIterator__int32
    var inline422 *ref_int32_x = source__4.conversions
    var inline423 *ref_int32_x = source__4.conversions
    var inline424 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline423)
    var inline425 int32 = inline424 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline422, inline425)
    var inline427 *_goml_vec_int32 = source__4.values
    var inline428 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline427)
    for_iter192 = inline428
    Loop_loop253:
    for {
        var for_next193 Option__int32
        var inline417 func() Option__int32 = for_iter192.next_fn
        var inline418 Option__int32 = inline417()
        for_next193 = inline418
        switch for_next193.(type) {
        case None:
            break Loop_loop253
        case Some:
            var x194 int32 = for_next193.(Some)._0
            var t255 int32
            var inline415 int32 = ref_get__Ref_5int32(total__5)
            t255 = inline415
            var t256 int32 = t255 + x194
            ref_set__Ref_5int32(total__5, t256)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline420 int32 = ref_get__Ref_5int32(total__5)
    return inline420
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline444 int32 = 0
    var inline445 *ref_int32_x = ref__Ref_5int32(inline444)
    total__5 = inline445
    var for_iter192 FnIterator__int32
    var inline442 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter192 = inline442
    Loop_loop261:
    for {
        var for_next193 Option__int32
        var inline437 func() Option__int32 = for_iter192.next_fn
        var inline438 Option__int32 = inline437()
        for_next193 = inline438
        switch for_next193.(type) {
        case None:
            break Loop_loop261
        case Some:
            var x194 int32 = for_next193.(Some)._0
            var t263 int32
            var inline435 int32 = ref_get__Ref_5int32(total__5)
            t263 = inline435
            var t264 int32 = t263 + x194
            ref_set__Ref_5int32(total__5, t264)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline440 int32 = ref_get__Ref_5int32(total__5)
    return inline440
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline458 int32 = 0
    var inline459 *ref_int32_x = ref__Ref_5int32(inline458)
    total__5 = inline459
    var for_iter192 FnIterator__int32
    var inline456 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter192 = inline456
    Loop_loop269:
    for {
        var for_next193 Option__int32
        var inline451 func() Option__int32 = for_iter192.next_fn
        var inline452 Option__int32 = inline451()
        for_next193 = inline452
        switch for_next193.(type) {
        case None:
            break Loop_loop269
        case Some:
            var x194 int32 = for_next193.(Some)._0
            var t271 int32
            var inline449 int32 = ref_get__Ref_5int32(total__5)
            t271 = inline449
            var t272 int32 = t271 + x194
            ref_set__Ref_5int32(total__5, t272)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline454 int32 = ref_get__Ref_5int32(total__5)
    return inline454
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t284 string = _goml_runtime_core_int32_to_string(self__70)
    return t284
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__256 []int32) FnIterator__int32 {
    var index__257 *ref_int_x = ref__Ref_3int(0)
    var len__258 int
    var inline475 int = len(self__256)
    len__258 = inline475
    var t297 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__257,
        len_1: len__258,
        self_2: self__256,
    }
    var t298 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t297)
    }
    var inline473 FnIterator__int32 = FnIterator__int32{
        next_fn: t298,
    }
    return inline473
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env205 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__231 *ref_int_x = env205.index_0
    var len__232 int = env205.len_1
    var self__230 *_goml_vec_int32 = env205.self_2
    var current__233 int = ref_get__Ref_3int(index__231)
    var t322 bool = current__233 < len__232
    if t322 {
        var value__234 int32 = vec_get__Vec_5int32(self__230, current__233)
        var t323 int = current__233 + 1
        ref_set__Ref_3int(index__231, t323)
        var t324 Option__int32 = Some{
            _0: value__234,
        }
        return t324
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env206 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__257 *ref_int_x = env206.index_0
    var len__258 int = env206.len_1
    var self__256 []int32 = env206.self_2
    var current__259 int = ref_get__Ref_3int(index__257)
    var t329 bool = current__259 < len__258
    if t329 {
        var value__260 int32
        var inline477 int32 = self__256[current__259]
        value__260 = inline477
        var t330 int = current__259 + 1
        ref_set__Ref_3int(index__257, t330)
        var t331 Option__int32 = Some{
            _0: value__260,
        }
        return t331
    } else {
        return None{}
    }
}

func main() {
    main0()
}
