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

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
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

type Token struct {}

type Any struct {}

type Counter struct {
    current *ref_int32_x
    end int32
}

type FnIterator__i32 struct {
    next_fn func() Option__i32
}

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_std_iter_map_A_i32_B_i32_I_Counter_4 struct {
    iterator_0 Counter
    map_fn_1 func(int32) int32
}

type closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5 struct {
    iterator_0 FnIterator__i32
    predicate_1 func(int32) bool
}

type closure_env_std_iter_take_I_FnIterator_i32_6 struct {
    remaining_0 *ref_int_x
    iterator_1 FnIterator__i32
}

type closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7 struct {
    iterator_0 FnIterator__isize
    map_fn_1 func(int) string
}

type closure_env_goml_builtin_range_8 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func _goml_m_trait__impl_i_Convert_i__l_i32_r__x40_Token_i_convert(self__0 Token) int32 {
    return 7
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    return "seven"
}

func main0() struct{} {
    var t458 Token = Token{}
    var t459 int32 = _goml_m_trait__impl_i_Convert_i__l_i32_r__x40_Token_i_convert(t458)
    println__T_i32(t459)
    var t460 Token = Token{}
    var t461 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t460)
    println__T_string(t461)
    var t462 Token = Token{}
    var converted__8 int32 = convert_to__T_i32__V_Token(t462)
    println__T_i32(converted__8)
    var t464 string
    t464 = "marked"
    println__T_string(t464)
    var t466 string
    t466 = "marked"
    println__T_string(t466)
    var t468 string
    t468 = "marked"
    var inline724 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline724)
    var t470 Counter
    var inline719 int32 = 0
    var inline720 int32 = 8
    var inline721 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(inline719)
    var inline722 Counter = Counter{
        current: inline721,
        end: inline720,
    }
    t470 = inline722
    var t471 closure_env_main_0 = closure_env_main_0{}
    var t472 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t471, p0)
    }
    var mapped__10 FnIterator__i32
    var inline715 closure_env_std_iter_map_A_i32_B_i32_I_Counter_4 = closure_env_std_iter_map_A_i32_B_i32_I_Counter_4{
        iterator_0: t470,
        map_fn_1: t472,
    }
    var inline716 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h645d7a9bc4d79b01cd03faf046af5461_nter__4_i_apply(inline715)
    }
    var inline717 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline716)
    mapped__10 = inline717
    var t473 closure_env_main_1 = closure_env_main_1{}
    var t474 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t473, p0)
    }
    var filtered__12 FnIterator__i32
    var inline711 closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5 = closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5{
        iterator_0: mapped__10,
        predicate_1: t474,
    }
    var inline712 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h8f163b2d5b8bf9739c89e2204772b07d__i32__5_i_apply(inline711)
    }
    var inline713 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline712)
    filtered__12 = inline713
    var limited__13 FnIterator__i32
    var inline702 int = 3
    var inline703 bool = inline702 > 0
    var inline705 int
    if inline703 {
        inline705 = inline702
    } else {
        inline705 = 0
    }
    var inline706 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(inline705)
    var inline707 closure_env_std_iter_take_I_FnIterator_i32_6 = closure_env_std_iter_take_I_FnIterator_i32_6{
        remaining_0: inline706,
        iterator_1: filtered__12,
    }
    var inline708 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_hbf515b0203b88ffdb3eaded6d77747ee__i32__6_i_apply(inline707)
    }
    var inline709 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline708)
    limited__13 = inline709
    var for_iter418 FnIterator__i32
    for_iter418 = limited__13
    Loop_loop490:
    for {
        var for_next419 Option__i32
        var inline680 func() Option__i32 = for_iter418.next_fn
        var inline681 Option__i32 = inline680()
        for_next419 = inline681
        switch for_next419._tag {
        case 0:
            break Loop_loop490
        case 1:
            var x420 int32 = for_next419._v1_0
            var inline677 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x420)
            _goml_runtime_core_string_println(inline677)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t477 FnIterator__isize
    var inline697 int = 1
    var inline698 int = 5
    var inline699 FnIterator__isize = __goml_builtin_range(inline697, inline698)
    t477 = inline699
    var t478 closure_env_main_2 = closure_env_main_2{}
    var t479 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t478, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t477, 0, t479)
    var inline694 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline694)
    var t482 FnIterator__isize
    var inline690 int = 1
    var inline691 int = 4
    var inline692 FnIterator__isize = __goml_builtin_range(inline690, inline691)
    t482 = inline692
    var t483 closure_env_main_3 = closure_env_main_3{}
    var t484 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t483, p0)
    }
    var t485 FnIterator__string
    var inline686 closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7 = closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7{
        iterator_0: t482,
        map_fn_1: t484,
    }
    var inline687 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_h646bacd23126c6108881c7c439733cbb_size__7_i_apply(inline686)
    }
    var inline688 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline687)
    t485 = inline688
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t485)
    var for_limit425 int = vec_len__Vec_6string(texts__19)
    var for_index426 int = 0
    Loop_loop487:
    for {
        var t488 bool = for_index426 < for_limit425
        if t488 {
            var for_item427 string = vec_get__Vec_6string(texts__19, for_index426)
            var t489 int = for_index426 + 1
            for_index426 = t489
            var inline683 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item427)
            _goml_runtime_core_string_println(inline683)
            continue
        } else {
            break Loop_loop487
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__431 int32) *ref_int32_x {
    var t494 *ref_int32_x = ref__Ref_5int32(value__431)
    return t494
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__432 *ref_int32_x) int32 {
    var t497 int32 = ref_get__Ref_5int32(self__432)
    return t497
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t501 string
    var inline730 string = _goml_runtime_core_int32_to_string(value__1)
    t501 = inline730
    _goml_runtime_core_string_println(t501)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t504 string
    t504 = value__1
    _goml_runtime_core_string_println(t504)
    return struct{}{}
}

func convert_to__T_i32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(iterator__48 FnIterator__isize, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr542:
    for {
        var mtmp43 Option__isize
        var inline742 func() Option__isize = iterator__48.next_fn
        var inline743 Option__isize = inline742()
        mtmp43 = inline743
        switch mtmp43._tag {
        case 0:
            break Loop_loop_expr542
        case 1:
            var x44 int = mtmp43._v1_0
            var t544 int = combine__50(accumulator__51, x44)
            accumulator__51 = t544
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline757 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline757
    Loop_loop_expr554:
    for {
        var mtmp47 Option__string
        var inline754 func() Option__string = iterator__53.next_fn
        var inline755 Option__string = inline754()
        mtmp47 = inline755
        switch mtmp47._tag {
        case 0:
            break Loop_loop_expr554
        case 1:
            var x48 string = mtmp47._v1_0
            vec_push__Vec_6string(values__54, x48)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return values__54
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t567 string = _goml_runtime_core_int32_to_string(self__154)
    return t567
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(next_fn__254 func() Option__i32) FnIterator__i32 {
    var t572 FnIterator__i32 = FnIterator__i32{
        next_fn: next_fn__254,
    }
    return t572
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t575 *ref_int_x = ref__Ref_3int(value__431)
    return t575
}

func __goml_builtin_range(start__503 int, end__504 int) FnIterator__isize {
    var current__505 *ref_int_x = ref__Ref_3int(start__503)
    var t587 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__505,
        end_1: end__504,
    }
    var t588 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t587)
    }
    var inline761 FnIterator__isize = FnIterator__isize{
        next_fn: t588,
    }
    return inline761
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t592 string = _goml_runtime_core_int_to_string(self__151)
    return t592
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__254 func() Option__string) FnIterator__string {
    var t604 FnIterator__string = FnIterator__string{
        next_fn: next_fn__254,
    }
    return t604
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env430 closure_env_main_0, value__9 int32) int32 {
    var t622 int32 = value__9 * 2
    return t622
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env431 closure_env_main_1, value__11 int32) bool {
    var t625 bool = value__11 > 4
    return t625
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env432 closure_env_main_2, total__15 int, value__16 int) int {
    var t628 int = total__15 + value__16
    return t628
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env433 closure_env_main_3, value__18 int) string {
    var t631 string
    var inline763 string = _goml_runtime_core_int_to_string(value__18)
    t631 = inline763
    var t632 string = "v" + t631
    return t632
}

func _goml_m_inherent_i_closure__en_h645d7a9bc4d79b01cd03faf046af5461_nter__4_i_apply(env434 closure_env_std_iter_map_A_i32_B_i32_I_Counter_4) Option__i32 {
    var iterator__4 Counter = env434.iterator_0
    var map_fn__5 func(int32) int32 = env434.map_fn_1
    var commute_field798 int32
    var inline765 *ref_int32_x = iterator__4.current
    var inline766 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline765)
    var inline767 int32 = iterator__4.end
    var inline768 bool = inline766 < inline767
    if inline768 {
        var inline769 *ref_int32_x = iterator__4.current
        var inline770 int32 = inline766 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline769, inline770)
        commute_field798 = inline766
        var t637 int32 = map_fn__5(commute_field798)
        var t638 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t637,
        }
        return t638
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h8f163b2d5b8bf9739c89e2204772b07d__i32__5_i_apply(env435 closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5) Option__i32 {
    var iterator__7 FnIterator__i32 = env435.iterator_0
    var predicate__8 func(int32) bool = env435.predicate_1
    for {
        var mtmp3 Option__i32
        var inline774 func() Option__i32 = iterator__7.next_fn
        var inline775 Option__i32 = inline774()
        mtmp3 = inline775
        switch mtmp3._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x4 int32 = mtmp3._v1_0
            var t646 bool = predicate__8(x4)
            if t646 {
                var t647 Option__i32 = Option__i32{
                    _tag: 1,
                    _v1_0: x4,
                }
                return t647
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_hbf515b0203b88ffdb3eaded6d77747ee__i32__6_i_apply(env436 closure_env_std_iter_take_I_FnIterator_i32_6) Option__i32 {
    var remaining__16 *ref_int_x = env436.remaining_0
    var iterator__14 FnIterator__i32 = env436.iterator_1
    var t652 int
    var inline784 int = ref_get__Ref_3int(remaining__16)
    t652 = inline784
    var t653 bool = t652 == 0
    if t653 {
        return Option__i32{
            _tag: 0,
        }
    } else {
        var t654 int
        var inline782 int = ref_get__Ref_3int(remaining__16)
        t654 = inline782
        var t655 int = t654 - 1
        ref_set__Ref_3int(remaining__16, t655)
        var inline777 func() Option__i32 = iterator__14.next_fn
        var inline778 Option__i32 = inline777()
        return inline778
    }
}

func _goml_m_inherent_i_closure__en_h646bacd23126c6108881c7c439733cbb_size__7_i_apply(env437 closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7) Option__string {
    var iterator__4 FnIterator__isize = env437.iterator_0
    var map_fn__5 func(int) string = env437.map_fn_1
    var mtmp1 Option__isize
    var inline786 func() Option__isize = iterator__4.next_fn
    var inline787 Option__isize = inline786()
    mtmp1 = inline787
    switch mtmp1._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x2 int = mtmp1._v1_0
        var t661 string = map_fn__5(x2)
        var t662 Option__string = Option__string{
            _tag: 1,
            _v1_0: t661,
        }
        return t662
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env438 closure_env_goml_builtin_range_8) Option__isize {
    var current__505 *ref_int_x = env438.current_0
    var end__504 int = env438.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t667 bool = value__506 < end__504
    if t667 {
        var t668 int = value__506 + 1
        ref_set__Ref_3int(current__505, t668)
        var t669 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__506,
        }
        return t669
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
