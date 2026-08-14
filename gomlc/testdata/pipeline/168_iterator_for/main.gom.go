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
    var t248 int32
    var inline416 int32 = ref_get__Ref_5int32(calls__3)
    t248 = inline416
    var t249 int32 = t248 + 1
    ref_set__Ref_5int32(calls__3, t249)
    var inline410 int = 1
    var inline411 int = 5
    var inline412 FnIterator__int = __goml_builtin_range(inline410, inline411)
    return inline412
}

func first_even(values__4 FnIterator__int) int {
    var for_iter184 FnIterator__int
    for_iter184 = values__4
    Loop_loop254:
    for {
        var for_next185 Option__int
        var inline418 func() Option__int = for_iter184.next_fn
        var inline419 Option__int = inline418()
        for_next185 = inline419
        switch for_next185.(type) {
        case Option__int_None:
            break Loop_loop254
        case Option__int_Some:
            var x186 int = for_next185.(Option__int_Some)._0
            var t257 int = x186 / 2
            var t258 int = t257 * 2
            var t259 bool = t258 == x186
            if t259 {
                return x186
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
    var for_limit193 int = vec_len__Vec_5int32(values__6)
    var for_index194 int = 0
    Loop_loop297:
    for {
        var t298 bool = for_index194 < for_limit193
        if t298 {
            var for_item195 int32 = vec_get__Vec_5int32(values__6, for_index194)
            var t299 int = for_index194 + 1
            for_index194 = t299
            var t303 bool = for_item195 == 20
            if t303 {
                continue
            } else {
                var t301 int32
                var inline424 int32 = ref_get__Ref_5int32(sum__7)
                t301 = inline424
                var t302 int32 = t301 + for_item195
                ref_set__Ref_5int32(sum__7, t302)
                continue
            }
        } else {
            break Loop_loop297
        }
    }
    var t262 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t262)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t263 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t263)
    var t264 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t264)
    var for_limit204 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index205 int = 0
    Loop_loop292:
    for {
        var t293 bool = for_index205 < for_limit204
        if t293 {
            var for_item206 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index205)
            var t294 int = for_index205 + 1
            for_index205 = t294
            var x208 int32 = for_item206._0
            var x209 string = for_item206._1
            var t295 string
            var inline429 string = _goml_runtime_core_int32_to_string(x208)
            t295 = inline429
            var t296 string = t295 + x209
            var inline426 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t296)
            _goml_runtime_core_string_println(inline426)
            continue
        } else {
            break Loop_loop292
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t266 FnIterator__int = counted_range(calls__12)
    var for_iter212 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t266)
    Loop_loop288:
    for {
        var for_next213 Option__int
        var inline435 func() Option__int = for_iter212.next_fn
        var inline436 Option__int = inline435()
        for_next213 = inline436
        switch for_next213.(type) {
        case Option__int_None:
            break Loop_loop288
        case Option__int_Some:
            var x214 int = for_next213.(Option__int_Some)._0
            var t290 int
            var inline433 int = ref_get__Ref_3int(range_sum__13)
            t290 = inline433
            var t291 int = t290 + x214
            ref_set__Ref_3int(range_sum__13, t291)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t268 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t268)
    var t269 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t269)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source219 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit220 int = len(for_source219)
    var for_index221 int = 0
    Loop_loop283:
    for {
        var t284 bool = for_index221 < for_limit220
        if t284 {
            var for_item222 int32 = for_source219[for_index221]
            var t285 int = for_index221 + 1
            for_index221 = t285
            var t286 int32
            var inline440 int32 = ref_get__Ref_5int32(slice_sum__15)
            t286 = inline440
            var t287 int32 = t286 + for_item222
            ref_set__Ref_5int32(slice_sum__15, t287)
            continue
        } else {
            break Loop_loop283
        }
    }
    var t271 int32
    var inline474 int32 = ref_get__Ref_5int32(slice_sum__15)
    t271 = inline474
    println__T_int32(t271)
    var t272 FnIterator__int32
    var inline468 int32 = 4
    var inline469 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline468)
    var inline470 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline469,
    }
    var inline471 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline470)
    }
    var inline472 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline471)
    t272 = inline472
    var for_iter227 FnIterator__int32
    for_iter227 = t272
    Loop_loop279:
    for {
        var for_next228 Option__int32
        var inline445 func() Option__int32 = for_iter227.next_fn
        var inline446 Option__int32 = inline445()
        for_next228 = inline446
        switch for_next228.(type) {
        case Option__int32_None:
            break Loop_loop279
        case Option__int32_Some:
            var x229 int32 = for_next228.(Option__int32_Some)._0
            var t282 bool = x229 == 2
            if t282 {
                break Loop_loop279
            } else {
                var inline442 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x229)
                _goml_runtime_core_string_println(inline442)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline463 int = 0
    var inline464 int = 0
    var inline465 FnIterator__int = __goml_builtin_range(inline463, inline464)
    empty__18 = inline465
    var for_iter233 FnIterator__int
    for_iter233 = empty__18
    Loop_loop277:
    for {
        var for_next234 Option__int
        var inline448 func() Option__int = for_iter233.next_fn
        var inline449 Option__int = inline448()
        for_next234 = inline449
        switch for_next234.(type) {
        case Option__int_None:
            break Loop_loop277
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t275 FnIterator__int
    var inline458 int = 3
    var inline459 int = 8
    var inline460 FnIterator__int = __goml_builtin_range(inline458, inline459)
    t275 = inline460
    var t276 int = first_even(t275)
    var inline455 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t276)
    _goml_runtime_core_string_println(inline455)
    var inline451 string = "done"
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline451)
    _goml_runtime_core_string_println(inline452)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t306 *ref_int32_x = ref__Ref_5int32(value__270)
    return t306
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t309 int32 = ref_get__Ref_5int32(self__271)
    return t309
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__170 func() Option__int32) FnIterator__int32 {
    var t314 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__170,
    }
    return t314
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__172 FnIterator__int) FnIterator__int {
    return self__172
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t326 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t326
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t330 string
    var inline481 string = _goml_runtime_core_int32_to_string(value__1)
    t330 = inline481
    _goml_runtime_core_string_println(t330)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t334 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t334
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__174 *_goml_vec_Tuple2_5int32_6string, elem__175 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t345 *ref_int_x = ref__Ref_3int(value__270)
    return t345
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t348 int = ref_get__Ref_3int(self__271)
    return t348
}

func println__T_int(value__1 int) struct{} {
    var t352 string
    var inline484 string = _goml_runtime_core_int_to_string(value__1)
    t352 = inline484
    _goml_runtime_core_string_println(t352)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__227 *_goml_vec_int32, start__228 int, end__229 int) []int32 {
    var t356 []int32 = self__227.items[start__228:end__229]
    return t356
}

func __goml_builtin_range(start__333 int, end__334 int) FnIterator__int {
    var current__335 *ref_int_x = ref__Ref_3int(start__333)
    var t365 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__335,
        end_1: end__334,
    }
    var t366 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t365)
    }
    var inline486 FnIterator__int = FnIterator__int{
        next_fn: t366,
    }
    return inline486
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t370 string = _goml_runtime_core_int32_to_string(self__70)
    return t370
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t375 string = _goml_runtime_core_int_to_string(self__67)
    return t375
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env239 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env239.current_0
    var value__2 int32
    var inline490 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline490
    var t395 bool = value__2 > 0
    if t395 {
        var t396 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t396)
        var t397 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t397
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env240 closure_env_goml_builtin_range_1) Option__int {
    var current__335 *ref_int_x = env240.current_0
    var end__334 int = env240.end_1
    var value__336 int = ref_get__Ref_3int(current__335)
    var t402 bool = value__336 < end__334
    if t402 {
        var t403 int = value__336 + 1
        ref_set__Ref_3int(current__335, t403)
        var t404 Option__int = Option__int_Some{
            _0: value__336,
        }
        return t404
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
