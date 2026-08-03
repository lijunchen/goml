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
    var inline350 int32 = 0
    var inline351 *ref_int32_x = ref__Ref_5int32(inline350)
    builds__7 = inline351
    var conversions__8 *ref_int32_x
    var inline347 int32 = 0
    var inline348 *ref_int32_x = ref__Ref_5int32(inline347)
    conversions__8 = inline348
    var t170 Numbers
    var inline338 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline339 int32 = inline338 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline339)
    var inline341 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline341, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline341, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline341, 3)
    var inline345 Numbers = Numbers{
        values: inline341,
        conversions: conversions__8,
    }
    t170 = inline345
    var t171 int32 = sum__S_Numbers(t170)
    var inline335 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t171)
    _goml_runtime_core_string_println(inline335)
    var t172 int32
    var inline333 int32 = ref_get__Ref_5int32(builds__7)
    t172 = inline333
    var inline330 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t172)
    _goml_runtime_core_string_println(inline330)
    var t173 int32
    var inline328 int32 = ref_get__Ref_5int32(conversions__8)
    t173 = inline328
    var inline325 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t173)
    _goml_runtime_core_string_println(inline325)
    var values__9 *_goml_vec_int32
    var inline323 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline323
    var inline320 int32 = 10
    vec_push__Vec_5int32(values__9, inline320)
    var inline317 int32 = 20
    vec_push__Vec_5int32(values__9, inline317)
    var inline314 int32 = 30
    vec_push__Vec_5int32(values__9, inline314)
    var t174 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline311 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t174)
    _goml_runtime_core_string_println(inline311)
    var t175 []int32
    var inline307 int = 1
    var inline308 int = 3
    var inline309 []int32 = values__9.items[inline307:inline308]
    t175 = inline309
    var t176 int32 = _goml_m_sum____S__Slice_l_int32_r_(t175)
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t176)
    _goml_runtime_core_string_println(inline304)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__216 *ref_int32_x) int32 {
    var t179 int32 = ref_get__Ref_5int32(self__216)
    return t179
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__217 *ref_int32_x, value__218 int32) struct{} {
    ref_set__Ref_5int32(self__217, value__218)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__186 *_goml_vec_int32) FnIterator__int32 {
    var index__187 *ref_int_x = ref__Ref_3int(0)
    var len__188 int
    var inline353 int = vec_len__Vec_5int32(self__186)
    len__188 = inline353
    var t184 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__187,
        len_1: len__188,
        self_2: self__186,
    }
    var t185 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t184)
    })
    return t185
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t188 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t188
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__134 *_goml_vec_int32, elem__135 int32) struct{} {
    vec_push__Vec_5int32(self__134, elem__135)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline374 int32 = 0
    var inline375 *ref_int32_x = ref__Ref_5int32(inline374)
    total__5 = inline375
    var for_iter141 FnIterator__int32
    var inline366 *ref_int32_x = source__4.conversions
    var inline367 *ref_int32_x = source__4.conversions
    var inline368 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline367)
    var inline369 int32 = inline368 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline366, inline369)
    var inline371 *_goml_vec_int32 = source__4.values
    var inline372 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline371)
    for_iter141 = inline372
    Loop_loop201:
    for {
        var for_next142 Option__int32
        var inline361 func() Option__int32 = for_iter141.next_fn
        var inline362 Option__int32 = inline361()
        for_next142 = inline362
        switch for_next142.(type) {
        case None:
            break Loop_loop201
        case Some:
            var x143 int32 = for_next142.(Some)._0
            var t203 int32
            var inline359 int32 = ref_get__Ref_5int32(total__5)
            t203 = inline359
            var t204 int32 = t203 + x143
            ref_set__Ref_5int32(total__5, t204)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline364 int32 = ref_get__Ref_5int32(total__5)
    return inline364
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline388 int32 = 0
    var inline389 *ref_int32_x = ref__Ref_5int32(inline388)
    total__5 = inline389
    var for_iter141 FnIterator__int32
    var inline386 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter141 = inline386
    Loop_loop209:
    for {
        var for_next142 Option__int32
        var inline381 func() Option__int32 = for_iter141.next_fn
        var inline382 Option__int32 = inline381()
        for_next142 = inline382
        switch for_next142.(type) {
        case None:
            break Loop_loop209
        case Some:
            var x143 int32 = for_next142.(Some)._0
            var t211 int32
            var inline379 int32 = ref_get__Ref_5int32(total__5)
            t211 = inline379
            var t212 int32 = t211 + x143
            ref_set__Ref_5int32(total__5, t212)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline384 int32 = ref_get__Ref_5int32(total__5)
    return inline384
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline402 int32 = 0
    var inline403 *ref_int32_x = ref__Ref_5int32(inline402)
    total__5 = inline403
    var for_iter141 FnIterator__int32
    var inline400 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter141 = inline400
    Loop_loop217:
    for {
        var for_next142 Option__int32
        var inline395 func() Option__int32 = for_iter141.next_fn
        var inline396 Option__int32 = inline395()
        for_next142 = inline396
        switch for_next142.(type) {
        case None:
            break Loop_loop217
        case Some:
            var x143 int32 = for_next142.(Some)._0
            var t219 int32
            var inline393 int32 = ref_get__Ref_5int32(total__5)
            t219 = inline393
            var t220 int32 = t219 + x143
            ref_set__Ref_5int32(total__5, t220)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline398 int32 = ref_get__Ref_5int32(total__5)
    return inline398
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__130 func() Option__int32) FnIterator__int32 {
    var t229 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__130,
    }
    return t229
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__72)
    return t232
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__198 []int32) FnIterator__int32 {
    var index__199 *ref_int_x = ref__Ref_3int(0)
    var len__200 int
    var inline415 int = len(self__198)
    len__200 = inline415
    var t245 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__199,
        len_1: len__200,
        self_2: self__198,
    }
    var t246 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t245)
    })
    return t246
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env154 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__187 *ref_int_x = env154.index_0
    var len__188 int = env154.len_1
    var self__186 *_goml_vec_int32 = env154.self_2
    var current__189 int = ref_get__Ref_3int(index__187)
    var t269 bool = current__189 < len__188
    if t269 {
        var value__190 int32 = vec_get__Vec_5int32(self__186, current__189)
        var t270 int = current__189 + 1
        ref_set__Ref_3int(index__187, t270)
        var t271 Option__int32 = Some{
            _0: value__190,
        }
        return t271
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env155 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__199 *ref_int_x = env155.index_0
    var len__200 int = env155.len_1
    var self__198 []int32 = env155.self_2
    var current__201 int = ref_get__Ref_3int(index__199)
    var t276 bool = current__201 < len__200
    if t276 {
        var value__202 int32
        var inline417 int32 = self__198[current__201]
        value__202 = inline417
        var t277 int = current__201 + 1
        ref_set__Ref_3int(index__199, t277)
        var t278 Option__int32 = Some{
            _0: value__202,
        }
        return t278
    } else {
        return None{}
    }
}

func main() {
    main0()
}
