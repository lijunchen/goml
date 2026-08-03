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
    var for_index178 int = 1
    var for_limit179 int = 4
    Loop_loop249:
    for {
        var t250 bool = for_index178 < for_limit179
        if t250 {
            var for_item180 int = for_index178
            var t251 int = for_index178 + 1
            for_index178 = t251
            var inline319 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item180)
            _goml_runtime_core_string_println(inline319)
            continue
        } else {
            break Loop_loop249
        }
    }
    var calls__5 *ref_int_x
    var inline370 int = 0
    var inline371 *ref_int_x = ref__Ref_3int(inline370)
    calls__5 = inline371
    var for_index184 int
    var inline365 int = 4
    var inline366 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline367 int = inline366 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline367)
    for_index184 = inline365
    var for_limit185 int
    var inline360 int = 6
    var inline361 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline362 int = inline361 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline362)
    for_limit185 = inline360
    var for_done186 bool = for_index184 > for_limit185
    Loop_loop242:
    for {
        var t243 bool = !for_done186
        if t243 {
            var for_item187 int = for_index184
            var t245 bool = for_index184 == for_limit185
            if t245 {
                for_done186 = true
            } else {
                var t247 int = for_index184 + 1
                for_index184 = t247
            }
            var inline322 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item187)
            _goml_runtime_core_string_println(inline322)
            continue
        } else {
            break Loop_loop242
        }
    }
    var for_index191 int = 3
    var for_limit192 int = 1
    var for_done193 bool = for_index191 > for_limit192
    Loop_loop235:
    for {
        var t236 bool = !for_done193
        if t236 {
            var for_item194 int = for_index191
            var t238 bool = for_index191 == for_limit192
            if t238 {
                for_done193 = true
            } else {
                var t240 int = for_index191 + 1
                for_index191 = t240
            }
            var inline325 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item194)
            _goml_runtime_core_string_println(inline325)
            continue
        } else {
            break Loop_loop235
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index198 int = maximum__8
    var for_done200 bool = for_index198 > maximum__8
    Loop_loop228:
    for {
        var t229 bool = !for_done200
        if t229 {
            var for_item201 int = for_index198
            var t231 bool = for_index198 == maximum__8
            if t231 {
                for_done200 = true
            } else {
                var t233 int = for_index198 + 1
                for_index198 = t233
            }
            var inline328 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item201)
            _goml_runtime_core_string_println(inline328)
            continue
        } else {
            break Loop_loop228
        }
    }
    var iterator__10 FnIterator__int
    var inline353 int = 8
    var inline354 int = 8
    var inline355 *ref_int_x = ref__Ref_3int(inline353)
    var inline356 *ref_bool_x = ref__Ref_4bool(false)
    var inline357 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline356,
        current_1: inline355,
        end_2: inline354,
    }
    var inline358 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline357)
    })
    iterator__10 = inline358
    var mtmp205 Option__int
    var inline350 func() Option__int = iterator__10.next_fn
    var inline351 Option__int = inline350()
    mtmp205 = inline351
    switch mtmp205.(type) {
    case None:
        var inline331 string = "missing"
        var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline331)
        _goml_runtime_core_string_println(inline332)
    case Some:
        var x206 int = mtmp205.(Some)._0
        var inline335 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x206)
        _goml_runtime_core_string_println(inline335)
    default:
        panic("non-exhaustive match")
    }
    var t224 int
    var inline348 int = ref_get__Ref_3int(calls__5)
    t224 = inline348
    var inline345 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t224)
    _goml_runtime_core_string_println(inline345)
    var t225 int32
    var inline341 int32 = 10
    var inline342 int32 = 20
    var inline343 int32 = inline341 + inline342
    t225 = inline343
    var inline338 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t225)
    _goml_runtime_core_string_println(inline338)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t254 int = ref_get__Ref_3int(self__237)
    return t254
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__238 *ref_int_x, value__239 int) struct{} {
    ref_set__Ref_3int(self__238, value__239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t279 string = _goml_runtime_core_int_to_string(self__69)
    return t279
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t285 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t285
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t290 string = _goml_runtime_core_int32_to_string(self__72)
    return t290
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env210 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__312 *ref_bool_x = env210.finished_0
    var current__311 *ref_int_x = env210.current_1
    var end__310 int = env210.end_2
    var t311 bool = ref_get__Ref_4bool(finished__312)
    var jp306 bool
    if t311 {
        jp306 = true
    } else {
        var t312 int = ref_get__Ref_3int(current__311)
        var t313 bool = t312 > end__310
        jp306 = t313
    }
    if jp306 {
        return None{}
    } else {
        var value__313 int = ref_get__Ref_3int(current__311)
        var t309 bool
        var inline378 bool = value__313 == end__310
        t309 = inline378
        if t309 {
            ref_set__Ref_4bool(finished__312, true)
        } else {
            var t310 int = value__313 + 1
            ref_set__Ref_3int(current__311, t310)
        }
        var t308 Option__int = Some{
            _0: value__313,
        }
        return t308
    }
}

func main() {
    main0()
}
