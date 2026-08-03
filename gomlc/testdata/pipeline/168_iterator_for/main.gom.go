package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

type _goml_vec_Tuple2_5int32_6string struct {
    items []Tuple2_5int32_6string
}

func vec_new__Vec_21Tuple2_5int32_6string() *_goml_vec_Tuple2_5int32_6string {
    return &_goml_vec_Tuple2_5int32_6string{
        items: nil,
    }
}

func vec_push__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, elem Tuple2_5int32_6string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, index int) Tuple2_5int32_6string {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string) int {
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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

func countdown(start__0 int32) FnIterator__int32 {
    var current__1 *ref_int32_x
    var inline399 *ref_int32_x = ref__Ref_5int32(start__0)
    current__1 = inline399
    var t238 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t239 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t238)
    })
    return t239
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t242 int32
    var inline407 int32 = ref_get__Ref_5int32(calls__3)
    t242 = inline407
    var t243 int32 = t242 + 1
    ref_set__Ref_5int32(calls__3, t243)
    var inline401 int = 1
    var inline402 int = 5
    var inline403 FnIterator__int = __goml_builtin_range(inline401, inline402)
    return inline403
}

func first_even(values__4 FnIterator__int) int {
    var for_iter179 FnIterator__int
    for_iter179 = values__4
    Loop_loop248:
    for {
        var for_next180 Option__int
        var inline411 func() Option__int = for_iter179.next_fn
        var inline412 Option__int = inline411()
        for_next180 = inline412
        switch for_next180.(type) {
        case Option__int_None:
            break Loop_loop248
        case Option__int_Some:
            var x181 int = for_next180.(Option__int_Some)._0
            var t251 int = x181 / 2
            var t252 int = t251 * 2
            var t253 bool
            var inline409 bool = t252 == x181
            t253 = inline409
            if t253 {
                return x181
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return -1
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_limit188 int = vec_len__Vec_5int32(values__6)
    var for_index189 int = 0
    Loop_loop291:
    for {
        var t292 bool = for_index189 < for_limit188
        if t292 {
            var for_item190 int32 = vec_get__Vec_5int32(values__6, for_index189)
            var t293 int = for_index189 + 1
            for_index189 = t293
            var t297 bool
            var inline419 int32 = 20
            var inline420 bool = for_item190 == inline419
            t297 = inline420
            if t297 {
                continue
            } else {
                var t295 int32
                var inline417 int32 = ref_get__Ref_5int32(sum__7)
                t295 = inline417
                var t296 int32 = t295 + for_item190
                ref_set__Ref_5int32(sum__7, t296)
                continue
            }
        } else {
            break Loop_loop291
        }
    }
    var t256 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t256)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t257 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t257)
    var t258 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t258)
    var for_limit199 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index200 int = 0
    Loop_loop286:
    for {
        var t287 bool = for_index200 < for_limit199
        if t287 {
            var for_item201 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index200)
            var t288 int = for_index200 + 1
            for_index200 = t288
            var x203 int32 = for_item201._0
            var x204 string = for_item201._1
            var t289 string
            var inline425 string = _goml_runtime_core_int32_to_string(x203)
            t289 = inline425
            var t290 string = t289 + x204
            var inline422 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t290)
            _goml_runtime_core_string_println(inline422)
            continue
        } else {
            break Loop_loop286
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t260 FnIterator__int = counted_range(calls__12)
    var for_iter207 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t260)
    Loop_loop282:
    for {
        var for_next208 Option__int
        var inline431 func() Option__int = for_iter207.next_fn
        var inline432 Option__int = inline431()
        for_next208 = inline432
        switch for_next208.(type) {
        case Option__int_None:
            break Loop_loop282
        case Option__int_Some:
            var x209 int = for_next208.(Option__int_Some)._0
            var t284 int
            var inline429 int = ref_get__Ref_3int(range_sum__13)
            t284 = inline429
            var t285 int = t284 + x209
            ref_set__Ref_3int(range_sum__13, t285)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t262 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t262)
    var t263 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t263)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source214 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit215 int = len(for_source214)
    var for_index216 int = 0
    Loop_loop277:
    for {
        var t278 bool = for_index216 < for_limit215
        if t278 {
            var for_item217 int32 = for_source214[for_index216]
            var t279 int = for_index216 + 1
            for_index216 = t279
            var t280 int32
            var inline436 int32 = ref_get__Ref_5int32(slice_sum__15)
            t280 = inline436
            var t281 int32 = t280 + for_item217
            ref_set__Ref_5int32(slice_sum__15, t281)
            continue
        } else {
            break Loop_loop277
        }
    }
    var t265 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    var inline467 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t265)
    _goml_runtime_core_string_println(inline467)
    var t266 FnIterator__int32 = countdown(4)
    var for_iter222 FnIterator__int32
    for_iter222 = t266
    Loop_loop273:
    for {
        var for_next223 Option__int32
        var inline444 func() Option__int32 = for_iter222.next_fn
        var inline445 Option__int32 = inline444()
        for_next223 = inline445
        switch for_next223.(type) {
        case Option__int32_None:
            break Loop_loop273
        case Option__int32_Some:
            var x224 int32 = for_next223.(Option__int32_Some)._0
            var t276 bool
            var inline441 int32 = 2
            var inline442 bool = x224 == inline441
            t276 = inline442
            if t276 {
                break Loop_loop273
            } else {
                var inline438 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x224)
                _goml_runtime_core_string_println(inline438)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline462 int = 0
    var inline463 int = 0
    var inline464 FnIterator__int = __goml_builtin_range(inline462, inline463)
    empty__18 = inline464
    var for_iter228 FnIterator__int
    for_iter228 = empty__18
    Loop_loop271:
    for {
        var for_next229 Option__int
        var inline447 func() Option__int = for_iter228.next_fn
        var inline448 Option__int = inline447()
        for_next229 = inline448
        switch for_next229.(type) {
        case Option__int_None:
            break Loop_loop271
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t269 FnIterator__int
    var inline457 int = 3
    var inline458 int = 8
    var inline459 FnIterator__int = __goml_builtin_range(inline457, inline458)
    t269 = inline459
    var t270 int = first_even(t269)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t270)
    _goml_runtime_core_string_println(inline454)
    var inline450 string = "done"
    var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline450)
    _goml_runtime_core_string_println(inline451)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t300 *ref_int32_x = ref__Ref_5int32(value__236)
    return t300
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t303 int32 = ref_get__Ref_5int32(self__237)
    return t303
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__130 func() Option__int32) FnIterator__int32 {
    var t308 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__130,
    }
    return t308
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__138 FnIterator__int) FnIterator__int {
    return self__138
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t323 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t323
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__155 *_goml_vec_int32, elem__156 int32) struct{} {
    vec_push__Vec_5int32(self__155, elem__156)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t330 string
    var inline474 string = _goml_runtime_core_int32_to_string(value__31)
    t330 = inline474
    _goml_runtime_core_string_println(t330)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t334 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t334
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__155 *_goml_vec_Tuple2_5int32_6string, elem__156 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t345 *ref_int_x = ref__Ref_3int(value__236)
    return t345
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t348 int = ref_get__Ref_3int(self__237)
    return t348
}

func println__T_int(value__31 int) struct{} {
    var t352 string
    var inline477 string = _goml_runtime_core_int_to_string(value__31)
    t352 = inline477
    _goml_runtime_core_string_println(t352)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__204 *_goml_vec_int32, start__205 int, end__206 int) []int32 {
    var t356 []int32 = self__204.items[start__205:end__206]
    return t356
}

func __goml_builtin_range(start__247 int, end__248 int) FnIterator__int {
    var current__249 *ref_int_x = ref__Ref_3int(start__247)
    var t365 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__249,
        end_1: end__248,
    }
    var t366 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t365)
    })
    return t366
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t369 string = _goml_runtime_core_int32_to_string(self__72)
    return t369
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t374 string = _goml_runtime_core_int_to_string(self__69)
    return t374
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t377 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t377
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env234 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env234.current_0
    var value__2 int32
    var inline481 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline481
    var t388 bool = value__2 > 0
    if t388 {
        var t389 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t389)
        var t390 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t390
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env235 closure_env_goml_builtin_range_1) Option__int {
    var current__249 *ref_int_x = env235.current_0
    var end__248 int = env235.end_1
    var value__250 int = ref_get__Ref_3int(current__249)
    var t395 bool = value__250 < end__248
    if t395 {
        var t396 int = value__250 + 1
        ref_set__Ref_3int(current__249, t396)
        var t397 Option__int = Option__int_Some{
            _0: value__250,
        }
        return t397
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
