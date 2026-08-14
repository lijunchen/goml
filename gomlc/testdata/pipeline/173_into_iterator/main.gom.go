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
    var inline399 int32 = 0
    var inline400 *ref_int32_x = ref__Ref_5int32(inline399)
    builds__7 = inline400
    var conversions__8 *ref_int32_x
    var inline396 int32 = 0
    var inline397 *ref_int32_x = ref__Ref_5int32(inline396)
    conversions__8 = inline397
    var t216 Numbers
    var inline387 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline388 int32 = inline387 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline388)
    var inline390 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline390, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline390, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline390, 3)
    var inline394 Numbers = Numbers{
        values: inline390,
        conversions: conversions__8,
    }
    t216 = inline394
    var t217 int32 = sum__S_Numbers(t216)
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
    _goml_runtime_core_string_println(inline384)
    var t218 int32
    var inline382 int32 = ref_get__Ref_5int32(builds__7)
    t218 = inline382
    var inline379 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t218)
    _goml_runtime_core_string_println(inline379)
    var t219 int32
    var inline377 int32 = ref_get__Ref_5int32(conversions__8)
    t219 = inline377
    var inline374 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t219)
    _goml_runtime_core_string_println(inline374)
    var values__9 *_goml_vec_int32
    var inline372 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline372
    var inline369 int32 = 10
    vec_push__Vec_5int32(values__9, inline369)
    var inline366 int32 = 20
    vec_push__Vec_5int32(values__9, inline366)
    var inline363 int32 = 30
    vec_push__Vec_5int32(values__9, inline363)
    var t220 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline360 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t220)
    _goml_runtime_core_string_println(inline360)
    var t221 []int32
    var inline356 int = 1
    var inline357 int = 3
    var inline358 []int32 = values__9.items[inline356:inline357]
    t221 = inline358
    var t222 int32 = _goml_m_sum____S__Slice_l_int32_r_(t221)
    var inline353 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t222)
    _goml_runtime_core_string_println(inline353)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t225 int32 = ref_get__Ref_5int32(self__271)
    return t225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__230 *_goml_vec_int32) FnIterator__int32 {
    var index__231 *ref_int_x = ref__Ref_3int(0)
    var len__232 int
    var inline404 int = vec_len__Vec_5int32(self__230)
    len__232 = inline404
    var t230 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__231,
        len_1: len__232,
        self_2: self__230,
    }
    var t231 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t230)
    }
    var inline402 FnIterator__int32 = FnIterator__int32{
        next_fn: t231,
    }
    return inline402
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t235 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t235
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline425 int32 = 0
    var inline426 *ref_int32_x = ref__Ref_5int32(inline425)
    total__5 = inline426
    var for_iter187 FnIterator__int32
    var inline417 *ref_int32_x = source__4.conversions
    var inline418 *ref_int32_x = source__4.conversions
    var inline419 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline418)
    var inline420 int32 = inline419 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline417, inline420)
    var inline422 *_goml_vec_int32 = source__4.values
    var inline423 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline422)
    for_iter187 = inline423
    Loop_loop248:
    for {
        var for_next188 Option__int32
        var inline412 func() Option__int32 = for_iter187.next_fn
        var inline413 Option__int32 = inline412()
        for_next188 = inline413
        switch for_next188.(type) {
        case None:
            break Loop_loop248
        case Some:
            var x189 int32 = for_next188.(Some)._0
            var t250 int32
            var inline410 int32 = ref_get__Ref_5int32(total__5)
            t250 = inline410
            var t251 int32 = t250 + x189
            ref_set__Ref_5int32(total__5, t251)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline415 int32 = ref_get__Ref_5int32(total__5)
    return inline415
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline439 int32 = 0
    var inline440 *ref_int32_x = ref__Ref_5int32(inline439)
    total__5 = inline440
    var for_iter187 FnIterator__int32
    var inline437 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter187 = inline437
    Loop_loop256:
    for {
        var for_next188 Option__int32
        var inline432 func() Option__int32 = for_iter187.next_fn
        var inline433 Option__int32 = inline432()
        for_next188 = inline433
        switch for_next188.(type) {
        case None:
            break Loop_loop256
        case Some:
            var x189 int32 = for_next188.(Some)._0
            var t258 int32
            var inline430 int32 = ref_get__Ref_5int32(total__5)
            t258 = inline430
            var t259 int32 = t258 + x189
            ref_set__Ref_5int32(total__5, t259)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline435 int32 = ref_get__Ref_5int32(total__5)
    return inline435
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline453 int32 = 0
    var inline454 *ref_int32_x = ref__Ref_5int32(inline453)
    total__5 = inline454
    var for_iter187 FnIterator__int32
    var inline451 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter187 = inline451
    Loop_loop264:
    for {
        var for_next188 Option__int32
        var inline446 func() Option__int32 = for_iter187.next_fn
        var inline447 Option__int32 = inline446()
        for_next188 = inline447
        switch for_next188.(type) {
        case None:
            break Loop_loop264
        case Some:
            var x189 int32 = for_next188.(Some)._0
            var t266 int32
            var inline444 int32 = ref_get__Ref_5int32(total__5)
            t266 = inline444
            var t267 int32 = t266 + x189
            ref_set__Ref_5int32(total__5, t267)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline449 int32 = ref_get__Ref_5int32(total__5)
    return inline449
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t279 string = _goml_runtime_core_int32_to_string(self__70)
    return t279
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__253 []int32) FnIterator__int32 {
    var index__254 *ref_int_x = ref__Ref_3int(0)
    var len__255 int
    var inline470 int = len(self__253)
    len__255 = inline470
    var t292 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__254,
        len_1: len__255,
        self_2: self__253,
    }
    var t293 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t292)
    }
    var inline468 FnIterator__int32 = FnIterator__int32{
        next_fn: t293,
    }
    return inline468
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env200 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__231 *ref_int_x = env200.index_0
    var len__232 int = env200.len_1
    var self__230 *_goml_vec_int32 = env200.self_2
    var current__233 int = ref_get__Ref_3int(index__231)
    var t317 bool = current__233 < len__232
    if t317 {
        var value__234 int32 = vec_get__Vec_5int32(self__230, current__233)
        var t318 int = current__233 + 1
        ref_set__Ref_3int(index__231, t318)
        var t319 Option__int32 = Some{
            _0: value__234,
        }
        return t319
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env201 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__254 *ref_int_x = env201.index_0
    var len__255 int = env201.len_1
    var self__253 []int32 = env201.self_2
    var current__256 int = ref_get__Ref_3int(index__254)
    var t324 bool = current__256 < len__255
    if t324 {
        var value__257 int32
        var inline472 int32 = self__253[current__256]
        value__257 = inline472
        var t325 int = current__256 + 1
        ref_set__Ref_3int(index__254, t325)
        var t326 Option__int32 = Some{
            _0: value__257,
        }
        return t326
    } else {
        return None{}
    }
}

func main() {
    main0()
}
