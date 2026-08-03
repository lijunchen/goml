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
    var inline391 int32 = 0
    var inline392 *ref_int32_x = ref__Ref_5int32(inline391)
    builds__7 = inline392
    var conversions__8 *ref_int32_x
    var inline388 int32 = 0
    var inline389 *ref_int32_x = ref__Ref_5int32(inline388)
    conversions__8 = inline389
    var t211 Numbers
    var inline379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline380 int32 = inline379 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline380)
    var inline382 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline382, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline382, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline382, 3)
    var inline386 Numbers = Numbers{
        values: inline382,
        conversions: conversions__8,
    }
    t211 = inline386
    var t212 int32 = sum__S_Numbers(t211)
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
    _goml_runtime_core_string_println(inline376)
    var t213 int32
    var inline374 int32 = ref_get__Ref_5int32(builds__7)
    t213 = inline374
    var inline371 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
    _goml_runtime_core_string_println(inline371)
    var t214 int32
    var inline369 int32 = ref_get__Ref_5int32(conversions__8)
    t214 = inline369
    var inline366 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
    _goml_runtime_core_string_println(inline366)
    var values__9 *_goml_vec_int32
    var inline364 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline364
    var inline361 int32 = 10
    vec_push__Vec_5int32(values__9, inline361)
    var inline358 int32 = 20
    vec_push__Vec_5int32(values__9, inline358)
    var inline355 int32 = 30
    vec_push__Vec_5int32(values__9, inline355)
    var t215 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline352 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t215)
    _goml_runtime_core_string_println(inline352)
    var t216 []int32
    var inline348 int = 1
    var inline349 int = 3
    var inline350 []int32 = values__9.items[inline348:inline349]
    t216 = inline350
    var t217 int32 = _goml_m_sum____S__Slice_l_int32_r_(t216)
    var inline345 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
    _goml_runtime_core_string_println(inline345)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t220 int32 = ref_get__Ref_5int32(self__237)
    return t220
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__238 *ref_int32_x, value__239 int32) struct{} {
    ref_set__Ref_5int32(self__238, value__239)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__207 *_goml_vec_int32) FnIterator__int32 {
    var index__208 *ref_int_x = ref__Ref_3int(0)
    var len__209 int
    var inline394 int = vec_len__Vec_5int32(self__207)
    len__209 = inline394
    var t225 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__208,
        len_1: len__209,
        self_2: self__207,
    }
    var t226 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t225)
    })
    return t226
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t229 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t229
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__155 *_goml_vec_int32, elem__156 int32) struct{} {
    vec_push__Vec_5int32(self__155, elem__156)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline415 int32 = 0
    var inline416 *ref_int32_x = ref__Ref_5int32(inline415)
    total__5 = inline416
    var for_iter182 FnIterator__int32
    var inline407 *ref_int32_x = source__4.conversions
    var inline408 *ref_int32_x = source__4.conversions
    var inline409 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline408)
    var inline410 int32 = inline409 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline407, inline410)
    var inline412 *_goml_vec_int32 = source__4.values
    var inline413 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline412)
    for_iter182 = inline413
    Loop_loop242:
    for {
        var for_next183 Option__int32
        var inline402 func() Option__int32 = for_iter182.next_fn
        var inline403 Option__int32 = inline402()
        for_next183 = inline403
        switch for_next183.(type) {
        case None:
            break Loop_loop242
        case Some:
            var x184 int32 = for_next183.(Some)._0
            var t244 int32
            var inline400 int32 = ref_get__Ref_5int32(total__5)
            t244 = inline400
            var t245 int32 = t244 + x184
            ref_set__Ref_5int32(total__5, t245)
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
    var for_iter182 FnIterator__int32
    var inline427 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter182 = inline427
    Loop_loop250:
    for {
        var for_next183 Option__int32
        var inline422 func() Option__int32 = for_iter182.next_fn
        var inline423 Option__int32 = inline422()
        for_next183 = inline423
        switch for_next183.(type) {
        case None:
            break Loop_loop250
        case Some:
            var x184 int32 = for_next183.(Some)._0
            var t252 int32
            var inline420 int32 = ref_get__Ref_5int32(total__5)
            t252 = inline420
            var t253 int32 = t252 + x184
            ref_set__Ref_5int32(total__5, t253)
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
    var for_iter182 FnIterator__int32
    var inline441 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter182 = inline441
    Loop_loop258:
    for {
        var for_next183 Option__int32
        var inline436 func() Option__int32 = for_iter182.next_fn
        var inline437 Option__int32 = inline436()
        for_next183 = inline437
        switch for_next183.(type) {
        case None:
            break Loop_loop258
        case Some:
            var x184 int32 = for_next183.(Some)._0
            var t260 int32
            var inline434 int32 = ref_get__Ref_5int32(total__5)
            t260 = inline434
            var t261 int32 = t260 + x184
            ref_set__Ref_5int32(total__5, t261)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline439 int32 = ref_get__Ref_5int32(total__5)
    return inline439
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__130 func() Option__int32) FnIterator__int32 {
    var t270 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__130,
    }
    return t270
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t273 string = _goml_runtime_core_int32_to_string(self__72)
    return t273
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__219 []int32) FnIterator__int32 {
    var index__220 *ref_int_x = ref__Ref_3int(0)
    var len__221 int
    var inline456 int = len(self__219)
    len__221 = inline456
    var t286 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__220,
        len_1: len__221,
        self_2: self__219,
    }
    var t287 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t286)
    })
    return t287
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env195 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__208 *ref_int_x = env195.index_0
    var len__209 int = env195.len_1
    var self__207 *_goml_vec_int32 = env195.self_2
    var current__210 int = ref_get__Ref_3int(index__208)
    var t310 bool = current__210 < len__209
    if t310 {
        var value__211 int32 = vec_get__Vec_5int32(self__207, current__210)
        var t311 int = current__210 + 1
        ref_set__Ref_3int(index__208, t311)
        var t312 Option__int32 = Some{
            _0: value__211,
        }
        return t312
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env196 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__220 *ref_int_x = env196.index_0
    var len__221 int = env196.len_1
    var self__219 []int32 = env196.self_2
    var current__222 int = ref_get__Ref_3int(index__220)
    var t317 bool = current__222 < len__221
    if t317 {
        var value__223 int32
        var inline458 int32 = self__219[current__222]
        value__223 = inline458
        var t318 int = current__222 + 1
        ref_set__Ref_3int(index__220, t318)
        var t319 Option__int32 = Some{
            _0: value__223,
        }
        return t319
    } else {
        return None{}
    }
}

func main() {
    main0()
}
