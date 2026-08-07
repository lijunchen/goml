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
    var inline358 *ref_int32_x = ref__Ref_5int32(start__0)
    current__1 = inline358
    var t197 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t198 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t197)
    })
    return t198
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t201 int32
    var inline366 int32 = ref_get__Ref_5int32(calls__3)
    t201 = inline366
    var t202 int32 = t201 + 1
    ref_set__Ref_5int32(calls__3, t202)
    var inline360 int = 1
    var inline361 int = 5
    var inline362 FnIterator__int = __goml_builtin_range(inline360, inline361)
    return inline362
}

func first_even(values__4 FnIterator__int) int {
    var for_iter138 FnIterator__int
    for_iter138 = values__4
    Loop_loop207:
    for {
        var for_next139 Option__int
        var inline370 func() Option__int = for_iter138.next_fn
        var inline371 Option__int = inline370()
        for_next139 = inline371
        switch for_next139.(type) {
        case Option__int_None:
            break Loop_loop207
        case Option__int_Some:
            var x140 int = for_next139.(Option__int_Some)._0
            var t210 int = x140 / 2
            var t211 int = t210 * 2
            var t212 bool
            var inline368 bool = t211 == x140
            t212 = inline368
            if t212 {
                return x140
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
    var for_limit147 int = vec_len__Vec_5int32(values__6)
    var for_index148 int = 0
    Loop_loop250:
    for {
        var t251 bool = for_index148 < for_limit147
        if t251 {
            var for_item149 int32 = vec_get__Vec_5int32(values__6, for_index148)
            var t252 int = for_index148 + 1
            for_index148 = t252
            var t256 bool
            var inline378 int32 = 20
            var inline379 bool = for_item149 == inline378
            t256 = inline379
            if t256 {
                continue
            } else {
                var t254 int32
                var inline376 int32 = ref_get__Ref_5int32(sum__7)
                t254 = inline376
                var t255 int32 = t254 + for_item149
                ref_set__Ref_5int32(sum__7, t255)
                continue
            }
        } else {
            break Loop_loop250
        }
    }
    var t215 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t215)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t216 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t216)
    var t217 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t217)
    var for_limit158 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index159 int = 0
    Loop_loop245:
    for {
        var t246 bool = for_index159 < for_limit158
        if t246 {
            var for_item160 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index159)
            var t247 int = for_index159 + 1
            for_index159 = t247
            var x162 int32 = for_item160._0
            var x163 string = for_item160._1
            var t248 string
            var inline384 string = _goml_runtime_core_int32_to_string(x162)
            t248 = inline384
            var t249 string = t248 + x163
            var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
            _goml_runtime_core_string_println(inline381)
            continue
        } else {
            break Loop_loop245
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t219 FnIterator__int = counted_range(calls__12)
    var for_iter166 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t219)
    Loop_loop241:
    for {
        var for_next167 Option__int
        var inline390 func() Option__int = for_iter166.next_fn
        var inline391 Option__int = inline390()
        for_next167 = inline391
        switch for_next167.(type) {
        case Option__int_None:
            break Loop_loop241
        case Option__int_Some:
            var x168 int = for_next167.(Option__int_Some)._0
            var t243 int
            var inline388 int = ref_get__Ref_3int(range_sum__13)
            t243 = inline388
            var t244 int = t243 + x168
            ref_set__Ref_3int(range_sum__13, t244)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t221 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t221)
    var t222 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t222)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source173 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit174 int = len(for_source173)
    var for_index175 int = 0
    Loop_loop236:
    for {
        var t237 bool = for_index175 < for_limit174
        if t237 {
            var for_item176 int32 = for_source173[for_index175]
            var t238 int = for_index175 + 1
            for_index175 = t238
            var t239 int32
            var inline395 int32 = ref_get__Ref_5int32(slice_sum__15)
            t239 = inline395
            var t240 int32 = t239 + for_item176
            ref_set__Ref_5int32(slice_sum__15, t240)
            continue
        } else {
            break Loop_loop236
        }
    }
    var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    var inline426 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t224)
    _goml_runtime_core_string_println(inline426)
    var t225 FnIterator__int32 = countdown(4)
    var for_iter181 FnIterator__int32
    for_iter181 = t225
    Loop_loop232:
    for {
        var for_next182 Option__int32
        var inline403 func() Option__int32 = for_iter181.next_fn
        var inline404 Option__int32 = inline403()
        for_next182 = inline404
        switch for_next182.(type) {
        case Option__int32_None:
            break Loop_loop232
        case Option__int32_Some:
            var x183 int32 = for_next182.(Option__int32_Some)._0
            var t235 bool
            var inline400 int32 = 2
            var inline401 bool = x183 == inline400
            t235 = inline401
            if t235 {
                break Loop_loop232
            } else {
                var inline397 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x183)
                _goml_runtime_core_string_println(inline397)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline421 int = 0
    var inline422 int = 0
    var inline423 FnIterator__int = __goml_builtin_range(inline421, inline422)
    empty__18 = inline423
    var for_iter187 FnIterator__int
    for_iter187 = empty__18
    Loop_loop230:
    for {
        var for_next188 Option__int
        var inline406 func() Option__int = for_iter187.next_fn
        var inline407 Option__int = inline406()
        for_next188 = inline407
        switch for_next188.(type) {
        case Option__int_None:
            break Loop_loop230
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t228 FnIterator__int
    var inline416 int = 3
    var inline417 int = 8
    var inline418 FnIterator__int = __goml_builtin_range(inline416, inline417)
    t228 = inline418
    var t229 int = first_even(t228)
    var inline413 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t229)
    _goml_runtime_core_string_println(inline413)
    var inline409 string = "done"
    var inline410 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline409)
    _goml_runtime_core_string_println(inline410)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__232 int32) *ref_int32_x {
    var t259 *ref_int32_x = ref__Ref_5int32(value__232)
    return t259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__233 *ref_int32_x) int32 {
    var t262 int32 = ref_get__Ref_5int32(self__233)
    return t262
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__147 func() Option__int32) FnIterator__int32 {
    var t267 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__147,
    }
    return t267
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__149 FnIterator__int) FnIterator__int {
    return self__149
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t282 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t282
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__151 *_goml_vec_int32, elem__152 int32) struct{} {
    vec_push__Vec_5int32(self__151, elem__152)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t289 string
    var inline433 string = _goml_runtime_core_int32_to_string(value__31)
    t289 = inline433
    _goml_runtime_core_string_println(t289)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t293 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t293
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__151 *_goml_vec_Tuple2_5int32_6string, elem__152 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__151, elem__152)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__232 int) *ref_int_x {
    var t304 *ref_int_x = ref__Ref_3int(value__232)
    return t304
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__233 *ref_int_x) int {
    var t307 int = ref_get__Ref_3int(self__233)
    return t307
}

func println__T_int(value__31 int) struct{} {
    var t311 string
    var inline436 string = _goml_runtime_core_int_to_string(value__31)
    t311 = inline436
    _goml_runtime_core_string_println(t311)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__200 *_goml_vec_int32, start__201 int, end__202 int) []int32 {
    var t315 []int32 = self__200.items[start__201:end__202]
    return t315
}

func __goml_builtin_range(start__243 int, end__244 int) FnIterator__int {
    var current__245 *ref_int_x = ref__Ref_3int(start__243)
    var t324 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__245,
        end_1: end__244,
    }
    var t325 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t324)
    })
    return t325
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t328 string = _goml_runtime_core_int32_to_string(self__72)
    return t328
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t333 string = _goml_runtime_core_int_to_string(self__69)
    return t333
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__147 func() Option__int) FnIterator__int {
    var t336 FnIterator__int = FnIterator__int{
        next_fn: next_fn__147,
    }
    return t336
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env193 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env193.current_0
    var value__2 int32
    var inline440 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline440
    var t347 bool = value__2 > 0
    if t347 {
        var t348 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t348)
        var t349 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t349
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env194 closure_env_goml_builtin_range_1) Option__int {
    var current__245 *ref_int_x = env194.current_0
    var end__244 int = env194.end_1
    var value__246 int = ref_get__Ref_3int(current__245)
    var t354 bool = value__246 < end__244
    if t354 {
        var t355 int = value__246 + 1
        ref_set__Ref_3int(current__245, t355)
        var t356 Option__int = Option__int_Some{
            _0: value__246,
        }
        return t356
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
