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
    var for_index173 int = 1
    var for_limit174 int = 4
    Loop_loop244:
    for {
        var t245 bool = for_index173 < for_limit174
        if t245 {
            var for_item175 int = for_index173
            var t246 int = for_index173 + 1
            for_index173 = t246
            var inline314 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item175)
            _goml_runtime_core_string_println(inline314)
            continue
        } else {
            break Loop_loop244
        }
    }
    var calls__5 *ref_int_x
    var inline365 int = 0
    var inline366 *ref_int_x = ref__Ref_3int(inline365)
    calls__5 = inline366
    var for_index179 int
    var inline360 int = 4
    var inline361 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline362 int = inline361 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline362)
    for_index179 = inline360
    var for_limit180 int
    var inline355 int = 6
    var inline356 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline357 int = inline356 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline357)
    for_limit180 = inline355
    var for_done181 bool = for_index179 > for_limit180
    Loop_loop237:
    for {
        var t238 bool = !for_done181
        if t238 {
            var for_item182 int = for_index179
            var t240 bool = for_index179 == for_limit180
            if t240 {
                for_done181 = true
            } else {
                var t242 int = for_index179 + 1
                for_index179 = t242
            }
            var inline317 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item182)
            _goml_runtime_core_string_println(inline317)
            continue
        } else {
            break Loop_loop237
        }
    }
    var for_index186 int = 3
    var for_limit187 int = 1
    var for_done188 bool = for_index186 > for_limit187
    Loop_loop230:
    for {
        var t231 bool = !for_done188
        if t231 {
            var for_item189 int = for_index186
            var t233 bool = for_index186 == for_limit187
            if t233 {
                for_done188 = true
            } else {
                var t235 int = for_index186 + 1
                for_index186 = t235
            }
            var inline320 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item189)
            _goml_runtime_core_string_println(inline320)
            continue
        } else {
            break Loop_loop230
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index193 int = maximum__8
    var for_done195 bool = for_index193 > maximum__8
    Loop_loop223:
    for {
        var t224 bool = !for_done195
        if t224 {
            var for_item196 int = for_index193
            var t226 bool = for_index193 == maximum__8
            if t226 {
                for_done195 = true
            } else {
                var t228 int = for_index193 + 1
                for_index193 = t228
            }
            var inline323 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item196)
            _goml_runtime_core_string_println(inline323)
            continue
        } else {
            break Loop_loop223
        }
    }
    var iterator__10 FnIterator__int
    var inline348 int = 8
    var inline349 int = 8
    var inline350 *ref_int_x = ref__Ref_3int(inline348)
    var inline351 *ref_bool_x = ref__Ref_4bool(false)
    var inline352 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline351,
        current_1: inline350,
        end_2: inline349,
    }
    var inline353 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline352)
    })
    iterator__10 = inline353
    var mtmp200 Option__int
    var inline345 func() Option__int = iterator__10.next_fn
    var inline346 Option__int = inline345()
    mtmp200 = inline346
    switch mtmp200.(type) {
    case None:
        var inline326 string = "missing"
        var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline326)
        _goml_runtime_core_string_println(inline327)
    case Some:
        var x201 int = mtmp200.(Some)._0
        var inline330 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x201)
        _goml_runtime_core_string_println(inline330)
    default:
        panic("non-exhaustive match")
    }
    var t219 int
    var inline343 int = ref_get__Ref_3int(calls__5)
    t219 = inline343
    var inline340 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t219)
    _goml_runtime_core_string_println(inline340)
    var t220 int32
    var inline336 int32 = 10
    var inline337 int32 = 20
    var inline338 int32 = inline336 + inline337
    t220 = inline338
    var inline333 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t220)
    _goml_runtime_core_string_println(inline333)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t249 int = ref_get__Ref_3int(self__258)
    return t249
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t274 string = _goml_runtime_core_int_to_string(self__69)
    return t274
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__172 func() Option__int) FnIterator__int {
    var t280 FnIterator__int = FnIterator__int{
        next_fn: next_fn__172,
    }
    return t280
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t285 string = _goml_runtime_core_int32_to_string(self__72)
    return t285
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env205 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__308 *ref_bool_x = env205.finished_0
    var current__307 *ref_int_x = env205.current_1
    var end__306 int = env205.end_2
    var t306 bool = ref_get__Ref_4bool(finished__308)
    var jp301 bool
    if t306 {
        jp301 = true
    } else {
        var t307 int = ref_get__Ref_3int(current__307)
        var t308 bool = t307 > end__306
        jp301 = t308
    }
    if jp301 {
        return None{}
    } else {
        var value__309 int = ref_get__Ref_3int(current__307)
        var t304 bool
        var inline373 bool = value__309 == end__306
        t304 = inline373
        if t304 {
            ref_set__Ref_4bool(finished__308, true)
        } else {
            var t305 int = value__309 + 1
            ref_set__Ref_3int(current__307, t305)
        }
        var t303 Option__int = Some{
            _0: value__309,
        }
        return t303
    }
}

func main() {
    main0()
}
