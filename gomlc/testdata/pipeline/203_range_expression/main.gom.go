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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_inclusive_0 struct {
    finished_0 *ref_bool_x
    current_1 *ref_int_x
    end_2 int
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func main0() struct{} {
    var for_index188 int = 1
    var for_limit189 int = 4
    Loop_loop259:
    for {
        var t260 bool = for_index188 < for_limit189
        if t260 {
            var for_item190 int = for_index188
            var t261 int = for_index188 + 1
            for_index188 = t261
            var inline330 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item190)
            _goml_runtime_core_string_println(inline330)
            continue
        } else {
            break Loop_loop259
        }
    }
    var calls__5 *ref_int_x
    var inline382 int = 0
    var inline383 *ref_int_x = ref__Ref_3int(inline382)
    calls__5 = inline383
    var for_index194 int
    var inline377 int = 4
    var inline378 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline379 int = inline378 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline379)
    for_index194 = inline377
    var for_limit195 int
    var inline372 int = 6
    var inline373 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline374 int = inline373 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline374)
    for_limit195 = inline372
    var for_done196 bool = for_index194 > for_limit195
    Loop_loop252:
    for {
        var t253 bool = !for_done196
        if t253 {
            var for_item197 int = for_index194
            var t255 bool = for_index194 == for_limit195
            if t255 {
                for_done196 = true
            } else {
                var t257 int = for_index194 + 1
                for_index194 = t257
            }
            var inline333 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item197)
            _goml_runtime_core_string_println(inline333)
            continue
        } else {
            break Loop_loop252
        }
    }
    var for_index201 int = 3
    var for_limit202 int = 1
    var for_done203 bool = for_index201 > for_limit202
    Loop_loop245:
    for {
        var t246 bool = !for_done203
        if t246 {
            var for_item204 int = for_index201
            var t248 bool = for_index201 == for_limit202
            if t248 {
                for_done203 = true
            } else {
                var t250 int = for_index201 + 1
                for_index201 = t250
            }
            var inline336 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item204)
            _goml_runtime_core_string_println(inline336)
            continue
        } else {
            break Loop_loop245
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index208 int = maximum__8
    var for_done210 bool = for_index208 > maximum__8
    Loop_loop238:
    for {
        var t239 bool = !for_done210
        if t239 {
            var for_item211 int = for_index208
            var t241 bool = for_index208 == maximum__8
            if t241 {
                for_done210 = true
            } else {
                var t243 int = for_index208 + 1
                for_index208 = t243
            }
            var inline339 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item211)
            _goml_runtime_core_string_println(inline339)
            continue
        } else {
            break Loop_loop238
        }
    }
    var iterator__10 FnIterator__int
    var inline364 int = 8
    var inline365 int = 8
    var inline366 *ref_int_x = ref__Ref_3int(inline364)
    var inline367 *ref_bool_x = ref__Ref_4bool(false)
    var inline368 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline367,
        current_1: inline366,
        end_2: inline365,
    }
    var inline369 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline368)
    }
    var inline370 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline369)
    iterator__10 = inline370
    var mtmp215 Option__int
    var inline361 func() Option__int = iterator__10.next_fn
    var inline362 Option__int = inline361()
    mtmp215 = inline362
    switch mtmp215.(type) {
    case None:
        var inline342 string = "missing"
        var inline343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline342)
        _goml_runtime_core_string_println(inline343)
    case Some:
        var x216 int = mtmp215.(Some)._0
        var inline346 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x216)
        _goml_runtime_core_string_println(inline346)
    default:
        panic("non-exhaustive match")
    }
    var t234 int
    var inline359 int = ref_get__Ref_3int(calls__5)
    t234 = inline359
    var inline356 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t234)
    _goml_runtime_core_string_println(inline356)
    var t235 int32
    var inline352 int32 = 10
    var inline353 int32 = 20
    var inline354 int32 = inline352 + inline353
    t235 = inline354
    var inline349 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t235)
    _goml_runtime_core_string_println(inline349)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t264 int = ref_get__Ref_3int(self__274)
    return t264
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__275 *ref_int_x, value__276 int) struct{} {
    ref_set__Ref_3int(self__275, value__276)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t290 string = _goml_runtime_core_int_to_string(self__67)
    return t290
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__170 func() Option__int) FnIterator__int {
    var t293 FnIterator__int = FnIterator__int{
        next_fn: next_fn__170,
    }
    return t293
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t298 string = _goml_runtime_core_int32_to_string(self__70)
    return t298
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env220 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__343 *ref_bool_x = env220.finished_0
    var current__342 *ref_int_x = env220.current_1
    var end__341 int = env220.end_2
    var t322 bool = ref_get__Ref_4bool(finished__343)
    var jp317 bool
    if t322 {
        jp317 = true
    } else {
        var t323 int = ref_get__Ref_3int(current__342)
        var t324 bool = t323 > end__341
        jp317 = t324
    }
    if jp317 {
        return None{}
    } else {
        var value__344 int = ref_get__Ref_3int(current__342)
        var t320 bool = value__344 == end__341
        if t320 {
            ref_set__Ref_4bool(finished__343, true)
        } else {
            var t321 int = value__344 + 1
            ref_set__Ref_3int(current__342, t321)
        }
        var t319 Option__int = Some{
            _0: value__344,
        }
        return t319
    }
}

func main() {
    main0()
}
