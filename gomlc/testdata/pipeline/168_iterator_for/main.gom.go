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
    var inline394 *ref_int32_x = ref__Ref_5int32(start__0)
    current__1 = inline394
    var t233 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t234 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t233)
    })
    return t234
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t237 int32
    var inline402 int32 = ref_get__Ref_5int32(calls__3)
    t237 = inline402
    var t238 int32 = t237 + 1
    ref_set__Ref_5int32(calls__3, t238)
    var inline396 int = 1
    var inline397 int = 5
    var inline398 FnIterator__int = __goml_builtin_range(inline396, inline397)
    return inline398
}

func first_even(values__4 FnIterator__int) int {
    var for_iter174 FnIterator__int
    for_iter174 = values__4
    Loop_loop243:
    for {
        var for_next175 Option__int
        var inline406 func() Option__int = for_iter174.next_fn
        var inline407 Option__int = inline406()
        for_next175 = inline407
        switch for_next175.(type) {
        case Option__int_None:
            break Loop_loop243
        case Option__int_Some:
            var x176 int = for_next175.(Option__int_Some)._0
            var t246 int = x176 / 2
            var t247 int = t246 * 2
            var t248 bool
            var inline404 bool = t247 == x176
            t248 = inline404
            if t248 {
                return x176
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
    var for_limit183 int = vec_len__Vec_5int32(values__6)
    var for_index184 int = 0
    Loop_loop286:
    for {
        var t287 bool = for_index184 < for_limit183
        if t287 {
            var for_item185 int32 = vec_get__Vec_5int32(values__6, for_index184)
            var t288 int = for_index184 + 1
            for_index184 = t288
            var t292 bool
            var inline414 int32 = 20
            var inline415 bool = for_item185 == inline414
            t292 = inline415
            if t292 {
                continue
            } else {
                var t290 int32
                var inline412 int32 = ref_get__Ref_5int32(sum__7)
                t290 = inline412
                var t291 int32 = t290 + for_item185
                ref_set__Ref_5int32(sum__7, t291)
                continue
            }
        } else {
            break Loop_loop286
        }
    }
    var t251 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t251)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t252 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t252)
    var t253 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t253)
    var for_limit194 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index195 int = 0
    Loop_loop281:
    for {
        var t282 bool = for_index195 < for_limit194
        if t282 {
            var for_item196 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index195)
            var t283 int = for_index195 + 1
            for_index195 = t283
            var x198 int32 = for_item196._0
            var x199 string = for_item196._1
            var t284 string
            var inline420 string = _goml_runtime_core_int32_to_string(x198)
            t284 = inline420
            var t285 string = t284 + x199
            var inline417 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t285)
            _goml_runtime_core_string_println(inline417)
            continue
        } else {
            break Loop_loop281
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t255 FnIterator__int = counted_range(calls__12)
    var for_iter202 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t255)
    Loop_loop277:
    for {
        var for_next203 Option__int
        var inline426 func() Option__int = for_iter202.next_fn
        var inline427 Option__int = inline426()
        for_next203 = inline427
        switch for_next203.(type) {
        case Option__int_None:
            break Loop_loop277
        case Option__int_Some:
            var x204 int = for_next203.(Option__int_Some)._0
            var t279 int
            var inline424 int = ref_get__Ref_3int(range_sum__13)
            t279 = inline424
            var t280 int = t279 + x204
            ref_set__Ref_3int(range_sum__13, t280)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t257 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t257)
    var t258 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t258)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source209 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit210 int = len(for_source209)
    var for_index211 int = 0
    Loop_loop272:
    for {
        var t273 bool = for_index211 < for_limit210
        if t273 {
            var for_item212 int32 = for_source209[for_index211]
            var t274 int = for_index211 + 1
            for_index211 = t274
            var t275 int32
            var inline431 int32 = ref_get__Ref_5int32(slice_sum__15)
            t275 = inline431
            var t276 int32 = t275 + for_item212
            ref_set__Ref_5int32(slice_sum__15, t276)
            continue
        } else {
            break Loop_loop272
        }
    }
    var t260 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    var inline462 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t260)
    _goml_runtime_core_string_println(inline462)
    var t261 FnIterator__int32 = countdown(4)
    var for_iter217 FnIterator__int32
    for_iter217 = t261
    Loop_loop268:
    for {
        var for_next218 Option__int32
        var inline439 func() Option__int32 = for_iter217.next_fn
        var inline440 Option__int32 = inline439()
        for_next218 = inline440
        switch for_next218.(type) {
        case Option__int32_None:
            break Loop_loop268
        case Option__int32_Some:
            var x219 int32 = for_next218.(Option__int32_Some)._0
            var t271 bool
            var inline436 int32 = 2
            var inline437 bool = x219 == inline436
            t271 = inline437
            if t271 {
                break Loop_loop268
            } else {
                var inline433 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x219)
                _goml_runtime_core_string_println(inline433)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline457 int = 0
    var inline458 int = 0
    var inline459 FnIterator__int = __goml_builtin_range(inline457, inline458)
    empty__18 = inline459
    var for_iter223 FnIterator__int
    for_iter223 = empty__18
    Loop_loop266:
    for {
        var for_next224 Option__int
        var inline442 func() Option__int = for_iter223.next_fn
        var inline443 Option__int = inline442()
        for_next224 = inline443
        switch for_next224.(type) {
        case Option__int_None:
            break Loop_loop266
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t264 FnIterator__int
    var inline452 int = 3
    var inline453 int = 8
    var inline454 FnIterator__int = __goml_builtin_range(inline452, inline453)
    t264 = inline454
    var t265 int = first_even(t264)
    var inline449 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t265)
    _goml_runtime_core_string_println(inline449)
    var inline445 string = "done"
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline445)
    _goml_runtime_core_string_println(inline446)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t295 *ref_int32_x = ref__Ref_5int32(value__257)
    return t295
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t298 int32 = ref_get__Ref_5int32(self__258)
    return t298
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t303 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t303
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__174 FnIterator__int) FnIterator__int {
    return self__174
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t318 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__176 *_goml_vec_int32, elem__177 int32) struct{} {
    vec_push__Vec_5int32(self__176, elem__177)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t325 string
    var inline469 string = _goml_runtime_core_int32_to_string(value__31)
    t325 = inline469
    _goml_runtime_core_string_println(t325)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t329 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t329
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__176 *_goml_vec_Tuple2_5int32_6string, elem__177 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t340 *ref_int_x = ref__Ref_3int(value__257)
    return t340
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t343 int = ref_get__Ref_3int(self__258)
    return t343
}

func println__T_int(value__31 int) struct{} {
    var t347 string
    var inline472 string = _goml_runtime_core_int_to_string(value__31)
    t347 = inline472
    _goml_runtime_core_string_println(t347)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__225 *_goml_vec_int32, start__226 int, end__227 int) []int32 {
    var t351 []int32 = self__225.items[start__226:end__227]
    return t351
}

func __goml_builtin_range(start__268 int, end__269 int) FnIterator__int {
    var current__270 *ref_int_x = ref__Ref_3int(start__268)
    var t360 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__270,
        end_1: end__269,
    }
    var t361 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t360)
    })
    return t361
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t364 string = _goml_runtime_core_int32_to_string(self__72)
    return t364
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t369 string = _goml_runtime_core_int_to_string(self__69)
    return t369
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__172 func() Option__int) FnIterator__int {
    var t372 FnIterator__int = FnIterator__int{
        next_fn: next_fn__172,
    }
    return t372
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env229 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env229.current_0
    var value__2 int32
    var inline476 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline476
    var t383 bool = value__2 > 0
    if t383 {
        var t384 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t384)
        var t385 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t385
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env230 closure_env_goml_builtin_range_1) Option__int {
    var current__270 *ref_int_x = env230.current_0
    var end__269 int = env230.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t390 bool = value__271 < end__269
    if t390 {
        var t391 int = value__271 + 1
        ref_set__Ref_3int(current__270, t391)
        var t392 Option__int = Option__int_Some{
            _0: value__271,
        }
        return t392
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
