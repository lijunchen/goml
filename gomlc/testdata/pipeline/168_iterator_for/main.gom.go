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

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t253 int32
    var inline421 int32 = ref_get__Ref_5int32(calls__3)
    t253 = inline421
    var t254 int32 = t253 + 1
    ref_set__Ref_5int32(calls__3, t254)
    var inline415 int = 1
    var inline416 int = 5
    var inline417 FnIterator__int = __goml_builtin_range(inline415, inline416)
    return inline417
}

func first_even(values__4 FnIterator__int) int {
    var for_iter189 FnIterator__int
    for_iter189 = values__4
    Loop_loop259:
    for {
        var for_next190 Option__int
        var inline423 func() Option__int = for_iter189.next_fn
        var inline424 Option__int = inline423()
        for_next190 = inline424
        switch for_next190.(type) {
        case Option__int_None:
            break Loop_loop259
        case Option__int_Some:
            var x191 int = for_next190.(Option__int_Some)._0
            var t262 int = x191 / 2
            var t263 int = t262 * 2
            var t264 bool = t263 == x191
            if t264 {
                return x191
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
    var for_limit198 int = vec_len__Vec_5int32(values__6)
    var for_index199 int = 0
    Loop_loop302:
    for {
        var t303 bool = for_index199 < for_limit198
        if t303 {
            var for_item200 int32 = vec_get__Vec_5int32(values__6, for_index199)
            var t304 int = for_index199 + 1
            for_index199 = t304
            var t308 bool = for_item200 == 20
            if t308 {
                continue
            } else {
                var t306 int32
                var inline429 int32 = ref_get__Ref_5int32(sum__7)
                t306 = inline429
                var t307 int32 = t306 + for_item200
                ref_set__Ref_5int32(sum__7, t307)
                continue
            }
        } else {
            break Loop_loop302
        }
    }
    var t267 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t267)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t268 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t268)
    var t269 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t269)
    var for_limit209 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index210 int = 0
    Loop_loop297:
    for {
        var t298 bool = for_index210 < for_limit209
        if t298 {
            var for_item211 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index210)
            var t299 int = for_index210 + 1
            for_index210 = t299
            var x213 int32 = for_item211._0
            var x214 string = for_item211._1
            var t300 string
            var inline434 string = _goml_runtime_core_int32_to_string(x213)
            t300 = inline434
            var t301 string = t300 + x214
            var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t301)
            _goml_runtime_core_string_println(inline431)
            continue
        } else {
            break Loop_loop297
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t271 FnIterator__int = counted_range(calls__12)
    var for_iter217 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t271)
    Loop_loop293:
    for {
        var for_next218 Option__int
        var inline440 func() Option__int = for_iter217.next_fn
        var inline441 Option__int = inline440()
        for_next218 = inline441
        switch for_next218.(type) {
        case Option__int_None:
            break Loop_loop293
        case Option__int_Some:
            var x219 int = for_next218.(Option__int_Some)._0
            var t295 int
            var inline438 int = ref_get__Ref_3int(range_sum__13)
            t295 = inline438
            var t296 int = t295 + x219
            ref_set__Ref_3int(range_sum__13, t296)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t273)
    var t274 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t274)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source224 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit225 int = len(for_source224)
    var for_index226 int = 0
    Loop_loop288:
    for {
        var t289 bool = for_index226 < for_limit225
        if t289 {
            var for_item227 int32 = for_source224[for_index226]
            var t290 int = for_index226 + 1
            for_index226 = t290
            var t291 int32
            var inline445 int32 = ref_get__Ref_5int32(slice_sum__15)
            t291 = inline445
            var t292 int32 = t291 + for_item227
            ref_set__Ref_5int32(slice_sum__15, t292)
            continue
        } else {
            break Loop_loop288
        }
    }
    var t276 int32
    var inline479 int32 = ref_get__Ref_5int32(slice_sum__15)
    t276 = inline479
    println__T_int32(t276)
    var t277 FnIterator__int32
    var inline473 int32 = 4
    var inline474 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline473)
    var inline475 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline474,
    }
    var inline476 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline475)
    }
    var inline477 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline476)
    t277 = inline477
    var for_iter232 FnIterator__int32
    for_iter232 = t277
    Loop_loop284:
    for {
        var for_next233 Option__int32
        var inline450 func() Option__int32 = for_iter232.next_fn
        var inline451 Option__int32 = inline450()
        for_next233 = inline451
        switch for_next233.(type) {
        case Option__int32_None:
            break Loop_loop284
        case Option__int32_Some:
            var x234 int32 = for_next233.(Option__int32_Some)._0
            var t287 bool = x234 == 2
            if t287 {
                break Loop_loop284
            } else {
                var inline447 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x234)
                _goml_runtime_core_string_println(inline447)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline468 int = 0
    var inline469 int = 0
    var inline470 FnIterator__int = __goml_builtin_range(inline468, inline469)
    empty__18 = inline470
    var for_iter238 FnIterator__int
    for_iter238 = empty__18
    Loop_loop282:
    for {
        var for_next239 Option__int
        var inline453 func() Option__int = for_iter238.next_fn
        var inline454 Option__int = inline453()
        for_next239 = inline454
        switch for_next239.(type) {
        case Option__int_None:
            break Loop_loop282
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t280 FnIterator__int
    var inline463 int = 3
    var inline464 int = 8
    var inline465 FnIterator__int = __goml_builtin_range(inline463, inline464)
    t280 = inline465
    var t281 int = first_even(t280)
    var inline460 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t281)
    _goml_runtime_core_string_println(inline460)
    var inline456 string = "done"
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline456)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t311 *ref_int32_x = ref__Ref_5int32(value__273)
    return t311
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t314 int32 = ref_get__Ref_5int32(self__274)
    return t314
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__170 func() Option__int32) FnIterator__int32 {
    var t319 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__170,
    }
    return t319
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__172 FnIterator__int) FnIterator__int {
    return self__172
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t331 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t331
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t335 string
    var inline486 string = _goml_runtime_core_int32_to_string(value__1)
    t335 = inline486
    _goml_runtime_core_string_println(t335)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t339 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t339
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__174 *_goml_vec_Tuple2_5int32_6string, elem__175 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t350 *ref_int_x = ref__Ref_3int(value__273)
    return t350
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t353 int = ref_get__Ref_3int(self__274)
    return t353
}

func println__T_int(value__1 int) struct{} {
    var t357 string
    var inline489 string = _goml_runtime_core_int_to_string(value__1)
    t357 = inline489
    _goml_runtime_core_string_println(t357)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__227 *_goml_vec_int32, start__228 int, end__229 int) []int32 {
    var t361 []int32 = self__227.items[start__228:end__229]
    return t361
}

func __goml_builtin_range(start__336 int, end__337 int) FnIterator__int {
    var current__338 *ref_int_x = ref__Ref_3int(start__336)
    var t370 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__338,
        end_1: end__337,
    }
    var t371 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t370)
    }
    var inline491 FnIterator__int = FnIterator__int{
        next_fn: t371,
    }
    return inline491
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t375 string = _goml_runtime_core_int32_to_string(self__70)
    return t375
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t380 string = _goml_runtime_core_int_to_string(self__67)
    return t380
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env244 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env244.current_0
    var value__2 int32
    var inline495 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline495
    var t400 bool = value__2 > 0
    if t400 {
        var t401 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t401)
        var t402 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t402
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env245 closure_env_goml_builtin_range_1) Option__int {
    var current__338 *ref_int_x = env245.current_0
    var end__337 int = env245.end_1
    var value__339 int = ref_get__Ref_3int(current__338)
    var t407 bool = value__339 < end__337
    if t407 {
        var t408 int = value__339 + 1
        ref_set__Ref_3int(current__338, t408)
        var t409 Option__int = Option__int_Some{
            _0: value__339,
        }
        return t409
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
