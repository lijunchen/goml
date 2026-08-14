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

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 struct {
    iterator_0 Counter
    map_fn_1 func(int32) int32
}

type closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 struct {
    iterator_0 FnIterator__int32
    predicate_1 func(int32) bool
}

type closure_env_std_iter_take_I_FnIterator_int32_6 struct {
    remaining_0 *ref_int_x
    iterator_1 FnIterator__int32
}

type closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 struct {
    iterator_0 FnIterator__int
    map_fn_1 func(int) string
}

type closure_env_goml_builtin_range_8 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

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

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(self__0 Token) int32 {
    return 7
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    return "seven"
}

func main0() struct{} {
    var t455 Token = Token{}
    var t456 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t455)
    println__T_int32(t456)
    var t457 Token = Token{}
    var t458 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t457)
    println__T_string(t458)
    var t459 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t459)
    println__T_int32(converted__8)
    var t461 string
    t461 = "marked"
    println__T_string(t461)
    var t463 string
    t463 = "marked"
    println__T_string(t463)
    var t465 string
    t465 = "marked"
    var inline721 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline721)
    var t467 Counter
    var inline716 int32 = 0
    var inline717 int32 = 8
    var inline718 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline716)
    var inline719 Counter = Counter{
        current: inline718,
        end: inline717,
    }
    t467 = inline719
    var t468 closure_env_main_0 = closure_env_main_0{}
    var t469 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t468, p0)
    }
    var mapped__10 FnIterator__int32
    var inline712 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: t467,
        map_fn_1: t469,
    }
    var inline713 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(inline712)
    }
    var inline714 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline713)
    mapped__10 = inline714
    var t470 closure_env_main_1 = closure_env_main_1{}
    var t471 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t470, p0)
    }
    var filtered__12 FnIterator__int32
    var inline708 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: mapped__10,
        predicate_1: t471,
    }
    var inline709 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(inline708)
    }
    var inline710 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline709)
    filtered__12 = inline710
    var limited__13 FnIterator__int32
    var inline699 int = 3
    var inline700 bool = inline699 > 0
    var inline702 int
    if inline700 {
        inline702 = inline699
    } else {
        inline702 = 0
    }
    var inline703 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline702)
    var inline704 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline703,
        iterator_1: filtered__12,
    }
    var inline705 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline704)
    }
    var inline706 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline705)
    limited__13 = inline706
    var for_iter415 FnIterator__int32
    for_iter415 = limited__13
    Loop_loop487:
    for {
        var for_next416 Option__int32
        var inline677 func() Option__int32 = for_iter415.next_fn
        var inline678 Option__int32 = inline677()
        for_next416 = inline678
        switch for_next416.(type) {
        case Option__int32_None:
            break Loop_loop487
        case Option__int32_Some:
            var x417 int32 = for_next416.(Option__int32_Some)._0
            var inline674 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x417)
            _goml_runtime_core_string_println(inline674)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t474 FnIterator__int
    var inline694 int = 1
    var inline695 int = 5
    var inline696 FnIterator__int = __goml_builtin_range(inline694, inline695)
    t474 = inline696
    var t475 closure_env_main_2 = closure_env_main_2{}
    var t476 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t475, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t474, 0, t476)
    var inline691 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline691)
    var t479 FnIterator__int
    var inline687 int = 1
    var inline688 int = 4
    var inline689 FnIterator__int = __goml_builtin_range(inline687, inline688)
    t479 = inline689
    var t480 closure_env_main_3 = closure_env_main_3{}
    var t481 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t480, p0)
    }
    var t482 FnIterator__string
    var inline683 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: t479,
        map_fn_1: t481,
    }
    var inline684 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(inline683)
    }
    var inline685 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline684)
    t482 = inline685
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t482)
    var for_limit422 int = vec_len__Vec_6string(texts__19)
    var for_index423 int = 0
    Loop_loop484:
    for {
        var t485 bool = for_index423 < for_limit422
        if t485 {
            var for_item424 string = vec_get__Vec_6string(texts__19, for_index423)
            var t486 int = for_index423 + 1
            for_index423 = t486
            var inline680 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item424)
            _goml_runtime_core_string_println(inline680)
            continue
        } else {
            break Loop_loop484
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t491 *ref_int32_x = ref__Ref_5int32(value__431)
    return t491
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t494 int32 = ref_get__Ref_5int32(self__432)
    return t494
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t498 string
    var inline727 string = _goml_runtime_core_int32_to_string(value__1)
    t498 = inline727
    _goml_runtime_core_string_println(t498)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t501 string
    t501 = value__1
    _goml_runtime_core_string_println(t501)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr539:
    for {
        var mtmp43 Option__int
        var inline739 func() Option__int = iterator__48.next_fn
        var inline740 Option__int = inline739()
        mtmp43 = inline740
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr539
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t541 int = combine__50(accumulator__51, x44)
            accumulator__51 = t541
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline754 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline754
    Loop_loop_expr551:
    for {
        var mtmp47 Option__string
        var inline751 func() Option__string = iterator__53.next_fn
        var inline752 Option__string = inline751()
        mtmp47 = inline752
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr551
        case Option__string_Some:
            var x48 string = mtmp47.(Option__string_Some)._0
            vec_push__Vec_6string(values__54, x48)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return values__54
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t564 string = _goml_runtime_core_int32_to_string(self__154)
    return t564
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__254 func() Option__int32) FnIterator__int32 {
    var t569 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__254,
    }
    return t569
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t572 *ref_int_x = ref__Ref_3int(value__431)
    return t572
}

func __goml_builtin_range(start__494 int, end__495 int) FnIterator__int {
    var current__496 *ref_int_x = ref__Ref_3int(start__494)
    var t584 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__496,
        end_1: end__495,
    }
    var t585 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t584)
    }
    var inline758 FnIterator__int = FnIterator__int{
        next_fn: t585,
    }
    return inline758
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t589 string = _goml_runtime_core_int_to_string(self__151)
    return t589
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__254 func() Option__string) FnIterator__string {
    var t601 FnIterator__string = FnIterator__string{
        next_fn: next_fn__254,
    }
    return t601
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env427 closure_env_main_0, value__9 int32) int32 {
    var t619 int32 = value__9 * 2
    return t619
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env428 closure_env_main_1, value__11 int32) bool {
    var t622 bool = value__11 > 4
    return t622
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env429 closure_env_main_2, total__15 int, value__16 int) int {
    var t625 int = total__15 + value__16
    return t625
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env430 closure_env_main_3, value__18 int) string {
    var t628 string
    var inline760 string = _goml_runtime_core_int_to_string(value__18)
    t628 = inline760
    var t629 string = "v" + t628
    return t629
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env431 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env431.iterator_0
    var map_fn__5 func(int32) int32 = env431.map_fn_1
    var commute_field795 int32
    var inline762 *ref_int32_x = iterator__4.current
    var inline763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline762)
    var inline764 int32 = iterator__4.end
    var inline765 bool = inline763 < inline764
    if inline765 {
        var inline766 *ref_int32_x = iterator__4.current
        var inline767 int32 = inline763 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline766, inline767)
        commute_field795 = inline763
        var t634 int32 = map_fn__5(commute_field795)
        var t635 Option__int32 = Option__int32_Some{
            _0: t634,
        }
        return t635
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env432 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env432.iterator_0
    var predicate__8 func(int32) bool = env432.predicate_1
    for {
        var mtmp3 Option__int32
        var inline771 func() Option__int32 = iterator__7.next_fn
        var inline772 Option__int32 = inline771()
        mtmp3 = inline772
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t643 bool = predicate__8(x4)
            if t643 {
                var t644 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t644
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env433 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env433.remaining_0
    var iterator__14 FnIterator__int32 = env433.iterator_1
    var t649 int
    var inline781 int = ref_get__Ref_3int(remaining__16)
    t649 = inline781
    var t650 bool = t649 == 0
    if t650 {
        return Option__int32_None{}
    } else {
        var t651 int
        var inline779 int = ref_get__Ref_3int(remaining__16)
        t651 = inline779
        var t652 int = t651 - 1
        ref_set__Ref_3int(remaining__16, t652)
        var inline774 func() Option__int32 = iterator__14.next_fn
        var inline775 Option__int32 = inline774()
        return inline775
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env434 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env434.iterator_0
    var map_fn__5 func(int) string = env434.map_fn_1
    var mtmp1 Option__int
    var inline783 func() Option__int = iterator__4.next_fn
    var inline784 Option__int = inline783()
    mtmp1 = inline784
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t658 string = map_fn__5(x2)
        var t659 Option__string = Option__string_Some{
            _0: t658,
        }
        return t659
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env435 closure_env_goml_builtin_range_8) Option__int {
    var current__496 *ref_int_x = env435.current_0
    var end__495 int = env435.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t664 bool = value__497 < end__495
    if t664 {
        var t665 int = value__497 + 1
        ref_set__Ref_3int(current__496, t665)
        var t666 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t666
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
