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

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

type Option__int struct {
    _tag int32
    _v1_0 int
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t474 int32
    var inline642 int32 = ref_get__Ref_5int32(calls__3)
    t474 = inline642
    var t475 int32 = t474 + 1
    ref_set__Ref_5int32(calls__3, t475)
    var inline636 int = 1
    var inline637 int = 5
    var inline638 FnIterator__int = __goml_builtin_range(inline636, inline637)
    return inline638
}

func first_even(values__4 FnIterator__int) int {
    var for_iter410 FnIterator__int
    for_iter410 = values__4
    Loop_loop480:
    for {
        var for_next411 Option__int
        var inline644 func() Option__int = for_iter410.next_fn
        var inline645 Option__int = inline644()
        for_next411 = inline645
        switch for_next411._tag {
        case 0:
            break Loop_loop480
        case 1:
            var x412 int = for_next411._v1_0
            var t483 int = x412 / 2
            var t484 int = t483 * 2
            var t485 bool = t484 == x412
            if t485 {
                return x412
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
    var for_limit419 int = vec_len__Vec_5int32(values__6)
    var for_index420 int = 0
    Loop_loop523:
    for {
        var t524 bool = for_index420 < for_limit419
        if t524 {
            var for_item421 int32 = vec_get__Vec_5int32(values__6, for_index420)
            var t525 int = for_index420 + 1
            for_index420 = t525
            var t529 bool = for_item421 == 20
            if t529 {
                continue
            } else {
                var t527 int32
                var inline650 int32 = ref_get__Ref_5int32(sum__7)
                t527 = inline650
                var t528 int32 = t527 + for_item421
                ref_set__Ref_5int32(sum__7, t528)
                continue
            }
        } else {
            break Loop_loop523
        }
    }
    var t488 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t488)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t489 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t489)
    var t490 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t490)
    var for_limit430 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index431 int = 0
    Loop_loop518:
    for {
        var t519 bool = for_index431 < for_limit430
        if t519 {
            var for_item432 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index431)
            var t520 int = for_index431 + 1
            for_index431 = t520
            var x434 int32 = for_item432._0
            var x435 string = for_item432._1
            var t521 string
            var inline655 string = _goml_runtime_core_int32_to_string(x434)
            t521 = inline655
            var t522 string = t521 + x435
            var inline652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t522)
            _goml_runtime_core_string_println(inline652)
            continue
        } else {
            break Loop_loop518
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t492 FnIterator__int = counted_range(calls__12)
    var for_iter438 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t492)
    Loop_loop514:
    for {
        var for_next439 Option__int
        var inline661 func() Option__int = for_iter438.next_fn
        var inline662 Option__int = inline661()
        for_next439 = inline662
        switch for_next439._tag {
        case 0:
            break Loop_loop514
        case 1:
            var x440 int = for_next439._v1_0
            var t516 int
            var inline659 int = ref_get__Ref_3int(range_sum__13)
            t516 = inline659
            var t517 int = t516 + x440
            ref_set__Ref_3int(range_sum__13, t517)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t494 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t494)
    var t495 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t495)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source445 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit446 int = len(for_source445)
    var for_index447 int = 0
    Loop_loop509:
    for {
        var t510 bool = for_index447 < for_limit446
        if t510 {
            var for_item448 int32 = for_source445[for_index447]
            var t511 int = for_index447 + 1
            for_index447 = t511
            var t512 int32
            var inline666 int32 = ref_get__Ref_5int32(slice_sum__15)
            t512 = inline666
            var t513 int32 = t512 + for_item448
            ref_set__Ref_5int32(slice_sum__15, t513)
            continue
        } else {
            break Loop_loop509
        }
    }
    var t497 int32
    var inline700 int32 = ref_get__Ref_5int32(slice_sum__15)
    t497 = inline700
    println__T_int32(t497)
    var t498 FnIterator__int32
    var inline694 int32 = 4
    var inline695 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline694)
    var inline696 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline695,
    }
    var inline697 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline696)
    }
    var inline698 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline697)
    t498 = inline698
    var for_iter453 FnIterator__int32
    for_iter453 = t498
    Loop_loop505:
    for {
        var for_next454 Option__int32
        var inline671 func() Option__int32 = for_iter453.next_fn
        var inline672 Option__int32 = inline671()
        for_next454 = inline672
        switch for_next454._tag {
        case 0:
            break Loop_loop505
        case 1:
            var x455 int32 = for_next454._v1_0
            var t508 bool = x455 == 2
            if t508 {
                break Loop_loop505
            } else {
                var inline668 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x455)
                _goml_runtime_core_string_println(inline668)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline689 int = 0
    var inline690 int = 0
    var inline691 FnIterator__int = __goml_builtin_range(inline689, inline690)
    empty__18 = inline691
    var for_iter459 FnIterator__int
    for_iter459 = empty__18
    Loop_loop503:
    for {
        var for_next460 Option__int
        var inline674 func() Option__int = for_iter459.next_fn
        var inline675 Option__int = inline674()
        for_next460 = inline675
        switch for_next460._tag {
        case 0:
            break Loop_loop503
        case 1:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t501 FnIterator__int
    var inline684 int = 3
    var inline685 int = 8
    var inline686 FnIterator__int = __goml_builtin_range(inline684, inline685)
    t501 = inline686
    var t502 int = first_even(t501)
    var inline681 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t502)
    _goml_runtime_core_string_println(inline681)
    var inline677 string = "done"
    var inline678 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline677)
    _goml_runtime_core_string_println(inline678)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t532 *ref_int32_x = ref__Ref_5int32(value__431)
    return t532
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t535 int32 = ref_get__Ref_5int32(self__432)
    return t535
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__254 func() Option__int32) FnIterator__int32 {
    var t540 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__254,
    }
    return t540
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__256 FnIterator__int) FnIterator__int {
    return self__256
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t552 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t552
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__258 *_goml_vec_int32, elem__259 int32) struct{} {
    vec_push__Vec_5int32(self__258, elem__259)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t556 string
    var inline707 string = _goml_runtime_core_int32_to_string(value__1)
    t556 = inline707
    _goml_runtime_core_string_println(t556)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t560 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t560
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__258 *_goml_vec_Tuple2_5int32_6string, elem__259 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t571 *ref_int_x = ref__Ref_3int(value__431)
    return t571
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t574 int = ref_get__Ref_3int(self__432)
    return t574
}

func println__T_int(value__1 int) struct{} {
    var t578 string
    var inline710 string = _goml_runtime_core_int_to_string(value__1)
    t578 = inline710
    _goml_runtime_core_string_println(t578)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__311 *_goml_vec_int32, start__312 int, end__313 int) []int32 {
    var t582 []int32 = self__311.items[start__312:end__313]
    return t582
}

func __goml_builtin_range(start__494 int, end__495 int) FnIterator__int {
    var current__496 *ref_int_x = ref__Ref_3int(start__494)
    var t591 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__496,
        end_1: end__495,
    }
    var t592 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t591)
    }
    var inline712 FnIterator__int = FnIterator__int{
        next_fn: t592,
    }
    return inline712
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t596 string = _goml_runtime_core_int32_to_string(self__154)
    return t596
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t601 string = _goml_runtime_core_int_to_string(self__151)
    return t601
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env465 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env465.current_0
    var value__2 int32
    var inline716 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline716
    var t621 bool = value__2 > 0
    if t621 {
        var t622 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t622)
        var t623 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__2,
        }
        return t623
    } else {
        return Option__int32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env466 closure_env_goml_builtin_range_1) Option__int {
    var current__496 *ref_int_x = env466.current_0
    var end__495 int = env466.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t628 bool = value__497 < end__495
    if t628 {
        var t629 int = value__497 + 1
        ref_set__Ref_3int(current__496, t629)
        var t630 Option__int = Option__int{
            _tag: 1,
            _v1_0: value__497,
        }
        return t630
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
