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
    var for_index183 int = 1
    var for_limit184 int = 4
    Loop_loop254:
    for {
        var t255 bool = for_index183 < for_limit184
        if t255 {
            var for_item185 int = for_index183
            var t256 int = for_index183 + 1
            for_index183 = t256
            var inline325 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item185)
            _goml_runtime_core_string_println(inline325)
            continue
        } else {
            break Loop_loop254
        }
    }
    var calls__5 *ref_int_x
    var inline377 int = 0
    var inline378 *ref_int_x = ref__Ref_3int(inline377)
    calls__5 = inline378
    var for_index189 int
    var inline372 int = 4
    var inline373 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline374 int = inline373 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline374)
    for_index189 = inline372
    var for_limit190 int
    var inline367 int = 6
    var inline368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline369 int = inline368 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline369)
    for_limit190 = inline367
    var for_done191 bool = for_index189 > for_limit190
    Loop_loop247:
    for {
        var t248 bool = !for_done191
        if t248 {
            var for_item192 int = for_index189
            var t250 bool = for_index189 == for_limit190
            if t250 {
                for_done191 = true
            } else {
                var t252 int = for_index189 + 1
                for_index189 = t252
            }
            var inline328 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item192)
            _goml_runtime_core_string_println(inline328)
            continue
        } else {
            break Loop_loop247
        }
    }
    var for_index196 int = 3
    var for_limit197 int = 1
    var for_done198 bool = for_index196 > for_limit197
    Loop_loop240:
    for {
        var t241 bool = !for_done198
        if t241 {
            var for_item199 int = for_index196
            var t243 bool = for_index196 == for_limit197
            if t243 {
                for_done198 = true
            } else {
                var t245 int = for_index196 + 1
                for_index196 = t245
            }
            var inline331 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item199)
            _goml_runtime_core_string_println(inline331)
            continue
        } else {
            break Loop_loop240
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index203 int = maximum__8
    var for_done205 bool = for_index203 > maximum__8
    Loop_loop233:
    for {
        var t234 bool = !for_done205
        if t234 {
            var for_item206 int = for_index203
            var t236 bool = for_index203 == maximum__8
            if t236 {
                for_done205 = true
            } else {
                var t238 int = for_index203 + 1
                for_index203 = t238
            }
            var inline334 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item206)
            _goml_runtime_core_string_println(inline334)
            continue
        } else {
            break Loop_loop233
        }
    }
    var iterator__10 FnIterator__int
    var inline359 int = 8
    var inline360 int = 8
    var inline361 *ref_int_x = ref__Ref_3int(inline359)
    var inline362 *ref_bool_x = ref__Ref_4bool(false)
    var inline363 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline362,
        current_1: inline361,
        end_2: inline360,
    }
    var inline364 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline363)
    }
    var inline365 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline364)
    iterator__10 = inline365
    var mtmp210 Option__int
    var inline356 func() Option__int = iterator__10.next_fn
    var inline357 Option__int = inline356()
    mtmp210 = inline357
    switch mtmp210.(type) {
    case None:
        var inline337 string = "missing"
        var inline338 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline337)
        _goml_runtime_core_string_println(inline338)
    case Some:
        var x211 int = mtmp210.(Some)._0
        var inline341 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x211)
        _goml_runtime_core_string_println(inline341)
    default:
        panic("non-exhaustive match")
    }
    var t229 int
    var inline354 int = ref_get__Ref_3int(calls__5)
    t229 = inline354
    var inline351 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t229)
    _goml_runtime_core_string_println(inline351)
    var t230 int32
    var inline347 int32 = 10
    var inline348 int32 = 20
    var inline349 int32 = inline347 + inline348
    t230 = inline349
    var inline344 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t230)
    _goml_runtime_core_string_println(inline344)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t259 int = ref_get__Ref_3int(self__271)
    return t259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__272 *ref_int_x, value__273 int) struct{} {
    ref_set__Ref_3int(self__272, value__273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t285 string = _goml_runtime_core_int_to_string(self__67)
    return t285
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__170 func() Option__int) FnIterator__int {
    var t288 FnIterator__int = FnIterator__int{
        next_fn: next_fn__170,
    }
    return t288
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t293 string = _goml_runtime_core_int32_to_string(self__70)
    return t293
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env215 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__340 *ref_bool_x = env215.finished_0
    var current__339 *ref_int_x = env215.current_1
    var end__338 int = env215.end_2
    var t317 bool = ref_get__Ref_4bool(finished__340)
    var jp312 bool
    if t317 {
        jp312 = true
    } else {
        var t318 int = ref_get__Ref_3int(current__339)
        var t319 bool = t318 > end__338
        jp312 = t319
    }
    if jp312 {
        return None{}
    } else {
        var value__341 int = ref_get__Ref_3int(current__339)
        var t315 bool = value__341 == end__338
        if t315 {
            ref_set__Ref_4bool(finished__340, true)
        } else {
            var t316 int = value__341 + 1
            ref_set__Ref_3int(current__339, t316)
        }
        var t314 Option__int = Some{
            _0: value__341,
        }
        return t314
    }
}

func main() {
    main0()
}
