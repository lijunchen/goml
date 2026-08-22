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

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type closure_env_goml_builtin_range_inclusive_0 struct {
    finished_0 *ref_bool_x
    current_1 *ref_int_x
    end_2 int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var for_index412 int = 1
    var for_limit413 int = 4
    Loop_loop483:
    for {
        var t484 bool = for_index412 < for_limit413
        if t484 {
            var for_item414 int = for_index412
            var t485 int = for_index412 + 1
            for_index412 = t485
            var inline554 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item414)
            _goml_runtime_core_string_println(inline554)
            continue
        } else {
            break Loop_loop483
        }
    }
    var calls__5 *ref_int_x
    var inline606 int = 0
    var inline607 *ref_int_x = ref__Ref_3int(inline606)
    calls__5 = inline607
    var for_index418 int
    var inline601 int = 4
    var inline602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(calls__5)
    var inline603 int = inline602 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(calls__5, inline603)
    for_index418 = inline601
    var for_limit419 int
    var inline596 int = 6
    var inline597 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(calls__5)
    var inline598 int = inline597 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(calls__5, inline598)
    for_limit419 = inline596
    var for_done420 bool = for_index418 > for_limit419
    Loop_loop476:
    for {
        var t477 bool = !for_done420
        if t477 {
            var for_item421 int = for_index418
            var t479 bool = for_index418 == for_limit419
            if t479 {
                for_done420 = true
            } else {
                var t481 int = for_index418 + 1
                for_index418 = t481
            }
            var inline557 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item421)
            _goml_runtime_core_string_println(inline557)
            continue
        } else {
            break Loop_loop476
        }
    }
    var for_index425 int = 3
    var for_limit426 int = 1
    var for_done427 bool = for_index425 > for_limit426
    Loop_loop469:
    for {
        var t470 bool = !for_done427
        if t470 {
            var for_item428 int = for_index425
            var t472 bool = for_index425 == for_limit426
            if t472 {
                for_done427 = true
            } else {
                var t474 int = for_index425 + 1
                for_index425 = t474
            }
            var inline560 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item428)
            _goml_runtime_core_string_println(inline560)
            continue
        } else {
            break Loop_loop469
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index432 int = maximum__8
    var for_done434 bool = for_index432 > maximum__8
    Loop_loop462:
    for {
        var t463 bool = !for_done434
        if t463 {
            var for_item435 int = for_index432
            var t465 bool = for_index432 == maximum__8
            if t465 {
                for_done434 = true
            } else {
                var t467 int = for_index432 + 1
                for_index432 = t467
            }
            var inline563 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item435)
            _goml_runtime_core_string_println(inline563)
            continue
        } else {
            break Loop_loop462
        }
    }
    var iterator__10 FnIterator__isize
    var inline588 int = 8
    var inline589 int = 8
    var inline590 *ref_int_x = ref__Ref_3int(inline588)
    var inline591 *ref_bool_x = ref__Ref_4bool(false)
    var inline592 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline591,
        current_1: inline590,
        end_2: inline589,
    }
    var inline593 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline592)
    }
    var inline594 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline593)
    iterator__10 = inline594
    var mtmp439 Option__isize
    var inline585 func() Option__isize = iterator__10.next_fn
    var inline586 Option__isize = inline585()
    mtmp439 = inline586
    switch mtmp439._tag {
    case 0:
        var inline566 string = "missing"
        var inline567 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline566)
        _goml_runtime_core_string_println(inline567)
    case 1:
        var x440 int = mtmp439._v1_0
        var inline570 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x440)
        _goml_runtime_core_string_println(inline570)
    default:
        panic("non-exhaustive match")
    }
    var t458 int
    var inline583 int = ref_get__Ref_3int(calls__5)
    t458 = inline583
    var inline580 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t458)
    _goml_runtime_core_string_println(inline580)
    var t459 int32
    var inline576 int32 = 10
    var inline577 int32 = 20
    var inline578 int32 = inline576 + inline577
    t459 = inline578
    var inline573 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t459)
    _goml_runtime_core_string_println(inline573)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__432 *ref_int_x) int {
    var t488 int = ref_get__Ref_3int(self__432)
    return t488
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t514 string = _goml_runtime_core_int_to_string(self__151)
    return t514
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__254 func() Option__isize) FnIterator__isize {
    var t517 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__254,
    }
    return t517
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t522 string = _goml_runtime_core_int32_to_string(self__154)
    return t522
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env444 closure_env_goml_builtin_range_inclusive_0) Option__isize {
    var finished__510 *ref_bool_x = env444.finished_0
    var current__509 *ref_int_x = env444.current_1
    var end__508 int = env444.end_2
    var t546 bool = ref_get__Ref_4bool(finished__510)
    var jp541 bool
    if t546 {
        jp541 = true
    } else {
        var t547 int = ref_get__Ref_3int(current__509)
        var t548 bool = t547 > end__508
        jp541 = t548
    }
    if jp541 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var value__511 int = ref_get__Ref_3int(current__509)
        var t544 bool = value__511 == end__508
        if t544 {
            ref_set__Ref_4bool(finished__510, true)
        } else {
            var t545 int = value__511 + 1
            ref_set__Ref_3int(current__509, t545)
        }
        var t543 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__511,
        }
        return t543
    }
}

func main() {
    main0()
}
