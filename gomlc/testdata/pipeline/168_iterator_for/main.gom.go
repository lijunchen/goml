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
    var t238 int32
    var inline406 int32 = ref_get__Ref_5int32(calls__3)
    t238 = inline406
    var t239 int32 = t238 + 1
    ref_set__Ref_5int32(calls__3, t239)
    var inline400 int = 1
    var inline401 int = 5
    var inline402 FnIterator__int = __goml_builtin_range(inline400, inline401)
    return inline402
}

func first_even(values__4 FnIterator__int) int {
    var for_iter174 FnIterator__int
    for_iter174 = values__4
    Loop_loop244:
    for {
        var for_next175 Option__int
        var inline408 func() Option__int = for_iter174.next_fn
        var inline409 Option__int = inline408()
        for_next175 = inline409
        switch for_next175.(type) {
        case Option__int_None:
            break Loop_loop244
        case Option__int_Some:
            var x176 int = for_next175.(Option__int_Some)._0
            var t247 int = x176 / 2
            var t248 int = t247 * 2
            var t249 bool = t248 == x176
            if t249 {
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
    Loop_loop287:
    for {
        var t288 bool = for_index184 < for_limit183
        if t288 {
            var for_item185 int32 = vec_get__Vec_5int32(values__6, for_index184)
            var t289 int = for_index184 + 1
            for_index184 = t289
            var t293 bool = for_item185 == 20
            if t293 {
                continue
            } else {
                var t291 int32
                var inline414 int32 = ref_get__Ref_5int32(sum__7)
                t291 = inline414
                var t292 int32 = t291 + for_item185
                ref_set__Ref_5int32(sum__7, t292)
                continue
            }
        } else {
            break Loop_loop287
        }
    }
    var t252 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t252)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t253 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t253)
    var t254 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t254)
    var for_limit194 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index195 int = 0
    Loop_loop282:
    for {
        var t283 bool = for_index195 < for_limit194
        if t283 {
            var for_item196 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index195)
            var t284 int = for_index195 + 1
            for_index195 = t284
            var x198 int32 = for_item196._0
            var x199 string = for_item196._1
            var t285 string
            var inline419 string = _goml_runtime_core_int32_to_string(x198)
            t285 = inline419
            var t286 string = t285 + x199
            var inline416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t286)
            _goml_runtime_core_string_println(inline416)
            continue
        } else {
            break Loop_loop282
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t256 FnIterator__int = counted_range(calls__12)
    var for_iter202 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t256)
    Loop_loop278:
    for {
        var for_next203 Option__int
        var inline425 func() Option__int = for_iter202.next_fn
        var inline426 Option__int = inline425()
        for_next203 = inline426
        switch for_next203.(type) {
        case Option__int_None:
            break Loop_loop278
        case Option__int_Some:
            var x204 int = for_next203.(Option__int_Some)._0
            var t280 int
            var inline423 int = ref_get__Ref_3int(range_sum__13)
            t280 = inline423
            var t281 int = t280 + x204
            ref_set__Ref_3int(range_sum__13, t281)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t258 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t258)
    var t259 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t259)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source209 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit210 int = len(for_source209)
    var for_index211 int = 0
    Loop_loop273:
    for {
        var t274 bool = for_index211 < for_limit210
        if t274 {
            var for_item212 int32 = for_source209[for_index211]
            var t275 int = for_index211 + 1
            for_index211 = t275
            var t276 int32
            var inline430 int32 = ref_get__Ref_5int32(slice_sum__15)
            t276 = inline430
            var t277 int32 = t276 + for_item212
            ref_set__Ref_5int32(slice_sum__15, t277)
            continue
        } else {
            break Loop_loop273
        }
    }
    var t261 int32
    var inline464 int32 = ref_get__Ref_5int32(slice_sum__15)
    t261 = inline464
    println__T_int32(t261)
    var t262 FnIterator__int32
    var inline458 int32 = 4
    var inline459 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline458)
    var inline460 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline459,
    }
    var inline461 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline460)
    }
    var inline462 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline461)
    t262 = inline462
    var for_iter217 FnIterator__int32
    for_iter217 = t262
    Loop_loop269:
    for {
        var for_next218 Option__int32
        var inline435 func() Option__int32 = for_iter217.next_fn
        var inline436 Option__int32 = inline435()
        for_next218 = inline436
        switch for_next218.(type) {
        case Option__int32_None:
            break Loop_loop269
        case Option__int32_Some:
            var x219 int32 = for_next218.(Option__int32_Some)._0
            var t272 bool = x219 == 2
            if t272 {
                break Loop_loop269
            } else {
                var inline432 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x219)
                _goml_runtime_core_string_println(inline432)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline453 int = 0
    var inline454 int = 0
    var inline455 FnIterator__int = __goml_builtin_range(inline453, inline454)
    empty__18 = inline455
    var for_iter223 FnIterator__int
    for_iter223 = empty__18
    Loop_loop267:
    for {
        var for_next224 Option__int
        var inline438 func() Option__int = for_iter223.next_fn
        var inline439 Option__int = inline438()
        for_next224 = inline439
        switch for_next224.(type) {
        case Option__int_None:
            break Loop_loop267
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t265 FnIterator__int
    var inline448 int = 3
    var inline449 int = 8
    var inline450 FnIterator__int = __goml_builtin_range(inline448, inline449)
    t265 = inline450
    var t266 int = first_even(t265)
    var inline445 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t266)
    _goml_runtime_core_string_println(inline445)
    var inline441 string = "done"
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline441)
    _goml_runtime_core_string_println(inline442)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t296 *ref_int32_x = ref__Ref_5int32(value__257)
    return t296
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t299 int32 = ref_get__Ref_5int32(self__258)
    return t299
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t304 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t304
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__174 FnIterator__int) FnIterator__int {
    return self__174
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t316 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t316
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__176 *_goml_vec_int32, elem__177 int32) struct{} {
    vec_push__Vec_5int32(self__176, elem__177)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t320 string
    var inline471 string = _goml_runtime_core_int32_to_string(value__31)
    t320 = inline471
    _goml_runtime_core_string_println(t320)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t324 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t324
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__176 *_goml_vec_Tuple2_5int32_6string, elem__177 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t335 *ref_int_x = ref__Ref_3int(value__257)
    return t335
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t338 int = ref_get__Ref_3int(self__258)
    return t338
}

func println__T_int(value__31 int) struct{} {
    var t342 string
    var inline474 string = _goml_runtime_core_int_to_string(value__31)
    t342 = inline474
    _goml_runtime_core_string_println(t342)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__225 *_goml_vec_int32, start__226 int, end__227 int) []int32 {
    var t346 []int32 = self__225.items[start__226:end__227]
    return t346
}

func __goml_builtin_range(start__268 int, end__269 int) FnIterator__int {
    var current__270 *ref_int_x = ref__Ref_3int(start__268)
    var t355 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__270,
        end_1: end__269,
    }
    var t356 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t355)
    }
    var inline476 FnIterator__int = FnIterator__int{
        next_fn: t356,
    }
    return inline476
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t360 string = _goml_runtime_core_int32_to_string(self__72)
    return t360
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t365 string = _goml_runtime_core_int_to_string(self__69)
    return t365
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env229 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env229.current_0
    var value__2 int32
    var inline480 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline480
    var t385 bool = value__2 > 0
    if t385 {
        var t386 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t386)
        var t387 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t387
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env230 closure_env_goml_builtin_range_1) Option__int {
    var current__270 *ref_int_x = env230.current_0
    var end__269 int = env230.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t392 bool = value__271 < end__269
    if t392 {
        var t393 int = value__271 + 1
        ref_set__Ref_3int(current__270, t393)
        var t394 Option__int = Option__int_Some{
            _0: value__271,
        }
        return t394
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
