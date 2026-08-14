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
    var t456 Token = Token{}
    var t457 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t456)
    println__T_int32(t457)
    var t458 Token = Token{}
    var t459 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t458)
    println__T_string(t459)
    var t460 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t460)
    println__T_int32(converted__8)
    var t462 string
    t462 = "marked"
    println__T_string(t462)
    var t464 string
    t464 = "marked"
    println__T_string(t464)
    var t466 string
    t466 = "marked"
    var inline722 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t466)
    _goml_runtime_core_string_println(inline722)
    var t468 Counter
    var inline717 int32 = 0
    var inline718 int32 = 8
    var inline719 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline717)
    var inline720 Counter = Counter{
        current: inline719,
        end: inline718,
    }
    t468 = inline720
    var t469 closure_env_main_0 = closure_env_main_0{}
    var t470 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t469, p0)
    }
    var mapped__10 FnIterator__int32
    var inline713 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: t468,
        map_fn_1: t470,
    }
    var inline714 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(inline713)
    }
    var inline715 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline714)
    mapped__10 = inline715
    var t471 closure_env_main_1 = closure_env_main_1{}
    var t472 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t471, p0)
    }
    var filtered__12 FnIterator__int32
    var inline709 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: mapped__10,
        predicate_1: t472,
    }
    var inline710 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(inline709)
    }
    var inline711 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline710)
    filtered__12 = inline711
    var limited__13 FnIterator__int32
    var inline700 int = 3
    var inline701 bool = inline700 > 0
    var inline703 int
    if inline701 {
        inline703 = inline700
    } else {
        inline703 = 0
    }
    var inline704 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline703)
    var inline705 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline704,
        iterator_1: filtered__12,
    }
    var inline706 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline705)
    }
    var inline707 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline706)
    limited__13 = inline707
    var for_iter415 FnIterator__int32
    for_iter415 = limited__13
    Loop_loop488:
    for {
        var for_next416 Option__int32
        var inline678 func() Option__int32 = for_iter415.next_fn
        var inline679 Option__int32 = inline678()
        for_next416 = inline679
        switch for_next416.(type) {
        case Option__int32_None:
            break Loop_loop488
        case Option__int32_Some:
            var x417 int32 = for_next416.(Option__int32_Some)._0
            var inline675 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x417)
            _goml_runtime_core_string_println(inline675)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t475 FnIterator__int
    var inline695 int = 1
    var inline696 int = 5
    var inline697 FnIterator__int = __goml_builtin_range(inline695, inline696)
    t475 = inline697
    var t476 closure_env_main_2 = closure_env_main_2{}
    var t477 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t476, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t475, 0, t477)
    var inline692 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline692)
    var t480 FnIterator__int
    var inline688 int = 1
    var inline689 int = 4
    var inline690 FnIterator__int = __goml_builtin_range(inline688, inline689)
    t480 = inline690
    var t481 closure_env_main_3 = closure_env_main_3{}
    var t482 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t481, p0)
    }
    var t483 FnIterator__string
    var inline684 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: t480,
        map_fn_1: t482,
    }
    var inline685 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(inline684)
    }
    var inline686 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline685)
    t483 = inline686
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t483)
    var for_limit422 int = vec_len__Vec_6string(texts__19)
    var for_index423 int = 0
    Loop_loop485:
    for {
        var t486 bool = for_index423 < for_limit422
        if t486 {
            var for_item424 string = vec_get__Vec_6string(texts__19, for_index423)
            var t487 int = for_index423 + 1
            for_index423 = t487
            var inline681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item424)
            _goml_runtime_core_string_println(inline681)
            continue
        } else {
            break Loop_loop485
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t492 *ref_int32_x = ref__Ref_5int32(value__431)
    return t492
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t495 int32 = ref_get__Ref_5int32(self__432)
    return t495
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t499 string
    var inline728 string = _goml_runtime_core_int32_to_string(value__1)
    t499 = inline728
    _goml_runtime_core_string_println(t499)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t502 string
    t502 = value__1
    _goml_runtime_core_string_println(t502)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr540:
    for {
        var mtmp43 Option__int
        var inline740 func() Option__int = iterator__48.next_fn
        var inline741 Option__int = inline740()
        mtmp43 = inline741
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr540
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t542 int = combine__50(accumulator__51, x44)
            accumulator__51 = t542
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline755 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline755
    Loop_loop_expr552:
    for {
        var mtmp47 Option__string
        var inline752 func() Option__string = iterator__53.next_fn
        var inline753 Option__string = inline752()
        mtmp47 = inline753
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr552
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
    var t565 string = _goml_runtime_core_int32_to_string(self__154)
    return t565
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__254 func() Option__int32) FnIterator__int32 {
    var t570 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__254,
    }
    return t570
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t573 *ref_int_x = ref__Ref_3int(value__431)
    return t573
}

func __goml_builtin_range(start__494 int, end__495 int) FnIterator__int {
    var current__496 *ref_int_x = ref__Ref_3int(start__494)
    var t585 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__496,
        end_1: end__495,
    }
    var t586 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t585)
    }
    var inline759 FnIterator__int = FnIterator__int{
        next_fn: t586,
    }
    return inline759
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t590 string = _goml_runtime_core_int_to_string(self__151)
    return t590
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__254 func() Option__string) FnIterator__string {
    var t602 FnIterator__string = FnIterator__string{
        next_fn: next_fn__254,
    }
    return t602
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env428 closure_env_main_0, value__9 int32) int32 {
    var t620 int32 = value__9 * 2
    return t620
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env429 closure_env_main_1, value__11 int32) bool {
    var t623 bool = value__11 > 4
    return t623
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env430 closure_env_main_2, total__15 int, value__16 int) int {
    var t626 int = total__15 + value__16
    return t626
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env431 closure_env_main_3, value__18 int) string {
    var t629 string
    var inline761 string = _goml_runtime_core_int_to_string(value__18)
    t629 = inline761
    var t630 string = "v" + t629
    return t630
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env432 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env432.iterator_0
    var map_fn__5 func(int32) int32 = env432.map_fn_1
    var commute_field796 int32
    var inline763 *ref_int32_x = iterator__4.current
    var inline764 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline763)
    var inline765 int32 = iterator__4.end
    var inline766 bool = inline764 < inline765
    if inline766 {
        var inline767 *ref_int32_x = iterator__4.current
        var inline768 int32 = inline764 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline767, inline768)
        commute_field796 = inline764
        var t635 int32 = map_fn__5(commute_field796)
        var t636 Option__int32 = Option__int32_Some{
            _0: t635,
        }
        return t636
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env433 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env433.iterator_0
    var predicate__8 func(int32) bool = env433.predicate_1
    for {
        var mtmp3 Option__int32
        var inline772 func() Option__int32 = iterator__7.next_fn
        var inline773 Option__int32 = inline772()
        mtmp3 = inline773
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t644 bool = predicate__8(x4)
            if t644 {
                var t645 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t645
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env434 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env434.remaining_0
    var iterator__14 FnIterator__int32 = env434.iterator_1
    var t650 int
    var inline782 int = ref_get__Ref_3int(remaining__16)
    t650 = inline782
    var t651 bool = t650 == 0
    if t651 {
        return Option__int32_None{}
    } else {
        var t652 int
        var inline780 int = ref_get__Ref_3int(remaining__16)
        t652 = inline780
        var t653 int = t652 - 1
        ref_set__Ref_3int(remaining__16, t653)
        var inline775 func() Option__int32 = iterator__14.next_fn
        var inline776 Option__int32 = inline775()
        return inline776
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env435 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env435.iterator_0
    var map_fn__5 func(int) string = env435.map_fn_1
    var mtmp1 Option__int
    var inline784 func() Option__int = iterator__4.next_fn
    var inline785 Option__int = inline784()
    mtmp1 = inline785
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t659 string = map_fn__5(x2)
        var t660 Option__string = Option__string_Some{
            _0: t659,
        }
        return t660
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env436 closure_env_goml_builtin_range_8) Option__int {
    var current__496 *ref_int_x = env436.current_0
    var end__495 int = env436.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t665 bool = value__497 < end__495
    if t665 {
        var t666 int = value__497 + 1
        ref_set__Ref_3int(current__496, t666)
        var t667 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t667
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
