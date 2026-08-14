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

type Ordering int32

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
    var for_index409 int = 1
    var for_limit410 int = 4
    Loop_loop480:
    for {
        var t481 bool = for_index409 < for_limit410
        if t481 {
            var for_item411 int = for_index409
            var t482 int = for_index409 + 1
            for_index409 = t482
            var inline551 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item411)
            _goml_runtime_core_string_println(inline551)
            continue
        } else {
            break Loop_loop480
        }
    }
    var calls__5 *ref_int_x
    var inline603 int = 0
    var inline604 *ref_int_x = ref__Ref_3int(inline603)
    calls__5 = inline604
    var for_index415 int
    var inline598 int = 4
    var inline599 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline600 int = inline599 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline600)
    for_index415 = inline598
    var for_limit416 int
    var inline593 int = 6
    var inline594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline595 int = inline594 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline595)
    for_limit416 = inline593
    var for_done417 bool = for_index415 > for_limit416
    Loop_loop473:
    for {
        var t474 bool = !for_done417
        if t474 {
            var for_item418 int = for_index415
            var t476 bool = for_index415 == for_limit416
            if t476 {
                for_done417 = true
            } else {
                var t478 int = for_index415 + 1
                for_index415 = t478
            }
            var inline554 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item418)
            _goml_runtime_core_string_println(inline554)
            continue
        } else {
            break Loop_loop473
        }
    }
    var for_index422 int = 3
    var for_limit423 int = 1
    var for_done424 bool = for_index422 > for_limit423
    Loop_loop466:
    for {
        var t467 bool = !for_done424
        if t467 {
            var for_item425 int = for_index422
            var t469 bool = for_index422 == for_limit423
            if t469 {
                for_done424 = true
            } else {
                var t471 int = for_index422 + 1
                for_index422 = t471
            }
            var inline557 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item425)
            _goml_runtime_core_string_println(inline557)
            continue
        } else {
            break Loop_loop466
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index429 int = maximum__8
    var for_done431 bool = for_index429 > maximum__8
    Loop_loop459:
    for {
        var t460 bool = !for_done431
        if t460 {
            var for_item432 int = for_index429
            var t462 bool = for_index429 == maximum__8
            if t462 {
                for_done431 = true
            } else {
                var t464 int = for_index429 + 1
                for_index429 = t464
            }
            var inline560 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item432)
            _goml_runtime_core_string_println(inline560)
            continue
        } else {
            break Loop_loop459
        }
    }
    var iterator__10 FnIterator__int
    var inline585 int = 8
    var inline586 int = 8
    var inline587 *ref_int_x = ref__Ref_3int(inline585)
    var inline588 *ref_bool_x = ref__Ref_4bool(false)
    var inline589 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline588,
        current_1: inline587,
        end_2: inline586,
    }
    var inline590 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline589)
    }
    var inline591 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline590)
    iterator__10 = inline591
    var mtmp436 Option__int
    var inline582 func() Option__int = iterator__10.next_fn
    var inline583 Option__int = inline582()
    mtmp436 = inline583
    switch mtmp436.(type) {
    case None:
        var inline563 string = "missing"
        var inline564 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline563)
        _goml_runtime_core_string_println(inline564)
    case Some:
        var x437 int = mtmp436.(Some)._0
        var inline567 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x437)
        _goml_runtime_core_string_println(inline567)
    default:
        panic("non-exhaustive match")
    }
    var t455 int
    var inline580 int = ref_get__Ref_3int(calls__5)
    t455 = inline580
    var inline577 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t455)
    _goml_runtime_core_string_println(inline577)
    var t456 int32
    var inline573 int32 = 10
    var inline574 int32 = 20
    var inline575 int32 = inline573 + inline574
    t456 = inline575
    var inline570 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t456)
    _goml_runtime_core_string_println(inline570)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t485 int = ref_get__Ref_3int(self__432)
    return t485
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t511 string = _goml_runtime_core_int_to_string(self__151)
    return t511
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t514 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t514
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t519 string = _goml_runtime_core_int32_to_string(self__154)
    return t519
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env441 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__501 *ref_bool_x = env441.finished_0
    var current__500 *ref_int_x = env441.current_1
    var end__499 int = env441.end_2
    var t543 bool = ref_get__Ref_4bool(finished__501)
    var jp538 bool
    if t543 {
        jp538 = true
    } else {
        var t544 int = ref_get__Ref_3int(current__500)
        var t545 bool = t544 > end__499
        jp538 = t545
    }
    if jp538 {
        return None{}
    } else {
        var value__502 int = ref_get__Ref_3int(current__500)
        var t541 bool = value__502 == end__499
        if t541 {
            ref_set__Ref_4bool(finished__501, true)
        } else {
            var t542 int = value__502 + 1
            ref_set__Ref_3int(current__500, t542)
        }
        var t540 Option__int = Some{
            _0: value__502,
        }
        return t540
    }
}

func main() {
    main0()
}
