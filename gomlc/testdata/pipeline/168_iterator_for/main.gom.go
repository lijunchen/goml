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
    var t477 int32
    var inline645 int32 = ref_get__Ref_5int32(calls__3)
    t477 = inline645
    var t478 int32 = t477 + 1
    ref_set__Ref_5int32(calls__3, t478)
    var inline639 int = 1
    var inline640 int = 5
    var inline641 FnIterator__int = __goml_builtin_range(inline639, inline640)
    return inline641
}

func first_even(values__4 FnIterator__int) int {
    var for_iter413 FnIterator__int
    for_iter413 = values__4
    Loop_loop483:
    for {
        var for_next414 Option__int
        var inline647 func() Option__int = for_iter413.next_fn
        var inline648 Option__int = inline647()
        for_next414 = inline648
        switch for_next414._tag {
        case 0:
            break Loop_loop483
        case 1:
            var x415 int = for_next414._v1_0
            var t486 int = x415 / 2
            var t487 int = t486 * 2
            var t488 bool = t487 == x415
            if t488 {
                return x415
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
    var for_limit422 int = vec_len__Vec_5int32(values__6)
    var for_index423 int = 0
    Loop_loop526:
    for {
        var t527 bool = for_index423 < for_limit422
        if t527 {
            var for_item424 int32 = vec_get__Vec_5int32(values__6, for_index423)
            var t528 int = for_index423 + 1
            for_index423 = t528
            var t532 bool = for_item424 == 20
            if t532 {
                continue
            } else {
                var t530 int32
                var inline653 int32 = ref_get__Ref_5int32(sum__7)
                t530 = inline653
                var t531 int32 = t530 + for_item424
                ref_set__Ref_5int32(sum__7, t531)
                continue
            }
        } else {
            break Loop_loop526
        }
    }
    var t491 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t491)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t492 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t492)
    var t493 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t493)
    var for_limit433 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index434 int = 0
    Loop_loop521:
    for {
        var t522 bool = for_index434 < for_limit433
        if t522 {
            var for_item435 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index434)
            var t523 int = for_index434 + 1
            for_index434 = t523
            var x437 int32 = for_item435._0
            var x438 string = for_item435._1
            var t524 string
            var inline658 string = _goml_runtime_core_int32_to_string(x437)
            t524 = inline658
            var t525 string = t524 + x438
            var inline655 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t525)
            _goml_runtime_core_string_println(inline655)
            continue
        } else {
            break Loop_loop521
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t495 FnIterator__int = counted_range(calls__12)
    var for_iter441 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t495)
    Loop_loop517:
    for {
        var for_next442 Option__int
        var inline664 func() Option__int = for_iter441.next_fn
        var inline665 Option__int = inline664()
        for_next442 = inline665
        switch for_next442._tag {
        case 0:
            break Loop_loop517
        case 1:
            var x443 int = for_next442._v1_0
            var t519 int
            var inline662 int = ref_get__Ref_3int(range_sum__13)
            t519 = inline662
            var t520 int = t519 + x443
            ref_set__Ref_3int(range_sum__13, t520)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t497 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t497)
    var t498 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t498)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source448 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit449 int = len(for_source448)
    var for_index450 int = 0
    Loop_loop512:
    for {
        var t513 bool = for_index450 < for_limit449
        if t513 {
            var for_item451 int32 = for_source448[for_index450]
            var t514 int = for_index450 + 1
            for_index450 = t514
            var t515 int32
            var inline669 int32 = ref_get__Ref_5int32(slice_sum__15)
            t515 = inline669
            var t516 int32 = t515 + for_item451
            ref_set__Ref_5int32(slice_sum__15, t516)
            continue
        } else {
            break Loop_loop512
        }
    }
    var t500 int32
    var inline703 int32 = ref_get__Ref_5int32(slice_sum__15)
    t500 = inline703
    println__T_int32(t500)
    var t501 FnIterator__int32
    var inline697 int32 = 4
    var inline698 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline697)
    var inline699 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline698,
    }
    var inline700 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline699)
    }
    var inline701 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline700)
    t501 = inline701
    var for_iter456 FnIterator__int32
    for_iter456 = t501
    Loop_loop508:
    for {
        var for_next457 Option__int32
        var inline674 func() Option__int32 = for_iter456.next_fn
        var inline675 Option__int32 = inline674()
        for_next457 = inline675
        switch for_next457._tag {
        case 0:
            break Loop_loop508
        case 1:
            var x458 int32 = for_next457._v1_0
            var t511 bool = x458 == 2
            if t511 {
                break Loop_loop508
            } else {
                var inline671 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x458)
                _goml_runtime_core_string_println(inline671)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int
    var inline692 int = 0
    var inline693 int = 0
    var inline694 FnIterator__int = __goml_builtin_range(inline692, inline693)
    empty__18 = inline694
    var for_iter462 FnIterator__int
    for_iter462 = empty__18
    Loop_loop506:
    for {
        var for_next463 Option__int
        var inline677 func() Option__int = for_iter462.next_fn
        var inline678 Option__int = inline677()
        for_next463 = inline678
        switch for_next463._tag {
        case 0:
            break Loop_loop506
        case 1:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t504 FnIterator__int
    var inline687 int = 3
    var inline688 int = 8
    var inline689 FnIterator__int = __goml_builtin_range(inline687, inline688)
    t504 = inline689
    var t505 int = first_even(t504)
    var inline684 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t505)
    _goml_runtime_core_string_println(inline684)
    var inline680 string = "done"
    var inline681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline680)
    _goml_runtime_core_string_println(inline681)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t535 *ref_int32_x = ref__Ref_5int32(value__431)
    return t535
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t538 int32 = ref_get__Ref_5int32(self__432)
    return t538
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__254 func() Option__int32) FnIterator__int32 {
    var t543 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__254,
    }
    return t543
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__256 FnIterator__int) FnIterator__int {
    return self__256
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t555 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t555
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__258 *_goml_vec_int32, elem__259 int32) struct{} {
    vec_push__Vec_5int32(self__258, elem__259)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t559 string
    var inline710 string = _goml_runtime_core_int32_to_string(value__1)
    t559 = inline710
    _goml_runtime_core_string_println(t559)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t563 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t563
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__258 *_goml_vec_Tuple2_5int32_6string, elem__259 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t574 *ref_int_x = ref__Ref_3int(value__431)
    return t574
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t577 int = ref_get__Ref_3int(self__432)
    return t577
}

func println__T_int(value__1 int) struct{} {
    var t581 string
    var inline713 string = _goml_runtime_core_int_to_string(value__1)
    t581 = inline713
    _goml_runtime_core_string_println(t581)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__311 *_goml_vec_int32, start__312 int, end__313 int) []int32 {
    var t585 []int32 = self__311.items[start__312:end__313]
    return t585
}

func __goml_builtin_range(start__503 int, end__504 int) FnIterator__int {
    var current__505 *ref_int_x = ref__Ref_3int(start__503)
    var t594 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__505,
        end_1: end__504,
    }
    var t595 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t594)
    }
    var inline715 FnIterator__int = FnIterator__int{
        next_fn: t595,
    }
    return inline715
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t599 string = _goml_runtime_core_int32_to_string(self__154)
    return t599
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t604 string = _goml_runtime_core_int_to_string(self__151)
    return t604
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env468 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env468.current_0
    var value__2 int32
    var inline719 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline719
    var t624 bool = value__2 > 0
    if t624 {
        var t625 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t625)
        var t626 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__2,
        }
        return t626
    } else {
        return Option__int32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env469 closure_env_goml_builtin_range_1) Option__int {
    var current__505 *ref_int_x = env469.current_0
    var end__504 int = env469.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t631 bool = value__506 < end__504
    if t631 {
        var t632 int = value__506 + 1
        ref_set__Ref_3int(current__505, t632)
        var t633 Option__int = Option__int{
            _tag: 1,
            _v1_0: value__506,
        }
        return t633
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
