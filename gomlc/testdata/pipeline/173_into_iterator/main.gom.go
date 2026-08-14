package main

import (
    _goml_fmt "fmt"
)

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

type Numbers struct {
    values *_goml_vec_int32
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 []int32
}

type Ordering int32

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    var builds__7 *ref_int32_x
    var inline625 int32 = 0
    var inline626 *ref_int32_x = ref__Ref_5int32(inline625)
    builds__7 = inline626
    var conversions__8 *ref_int32_x
    var inline622 int32 = 0
    var inline623 *ref_int32_x = ref__Ref_5int32(inline622)
    conversions__8 = inline623
    var t442 Numbers
    var inline613 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline614 int32 = inline613 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline614)
    var inline616 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline616, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline616, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline616, 3)
    var inline620 Numbers = Numbers{
        values: inline616,
        conversions: conversions__8,
    }
    t442 = inline620
    var t443 int32 = sum__S_Numbers(t442)
    var inline610 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t443)
    _goml_runtime_core_string_println(inline610)
    var t444 int32
    var inline608 int32 = ref_get__Ref_5int32(builds__7)
    t444 = inline608
    var inline605 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t444)
    _goml_runtime_core_string_println(inline605)
    var t445 int32
    var inline603 int32 = ref_get__Ref_5int32(conversions__8)
    t445 = inline603
    var inline600 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t445)
    _goml_runtime_core_string_println(inline600)
    var values__9 *_goml_vec_int32
    var inline598 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline598
    var inline595 int32 = 10
    vec_push__Vec_5int32(values__9, inline595)
    var inline592 int32 = 20
    vec_push__Vec_5int32(values__9, inline592)
    var inline589 int32 = 30
    vec_push__Vec_5int32(values__9, inline589)
    var t446 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline586 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t446)
    _goml_runtime_core_string_println(inline586)
    var t447 []int32
    var inline582 int = 1
    var inline583 int = 3
    var inline584 []int32 = values__9.items[inline582:inline583]
    t447 = inline584
    var t448 int32 = _goml_m_sum____S__Slice_l_int32_r_(t447)
    var inline579 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t448)
    _goml_runtime_core_string_println(inline579)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t451 int32 = ref_get__Ref_5int32(self__432)
    return t451
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__314 *_goml_vec_int32) FnIterator__int32 {
    var index__315 *ref_int_x = ref__Ref_3int(0)
    var len__316 int
    var inline630 int = vec_len__Vec_5int32(self__314)
    len__316 = inline630
    var t456 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__315,
        len_1: len__316,
        self_2: self__314,
    }
    var t457 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t456)
    }
    var inline628 FnIterator__int32 = FnIterator__int32{
        next_fn: t457,
    }
    return inline628
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t461 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t461
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__258 *_goml_vec_int32, elem__259 int32) struct{} {
    vec_push__Vec_5int32(self__258, elem__259)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline651 int32 = 0
    var inline652 *ref_int32_x = ref__Ref_5int32(inline651)
    total__5 = inline652
    var for_iter413 FnIterator__int32
    var inline643 *ref_int32_x = source__4.conversions
    var inline644 *ref_int32_x = source__4.conversions
    var inline645 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline644)
    var inline646 int32 = inline645 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline643, inline646)
    var inline648 *_goml_vec_int32 = source__4.values
    var inline649 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline648)
    for_iter413 = inline649
    Loop_loop474:
    for {
        var for_next414 Option__int32
        var inline638 func() Option__int32 = for_iter413.next_fn
        var inline639 Option__int32 = inline638()
        for_next414 = inline639
        switch for_next414.(type) {
        case None:
            break Loop_loop474
        case Some:
            var x415 int32 = for_next414.(Some)._0
            var t476 int32
            var inline636 int32 = ref_get__Ref_5int32(total__5)
            t476 = inline636
            var t477 int32 = t476 + x415
            ref_set__Ref_5int32(total__5, t477)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline641 int32 = ref_get__Ref_5int32(total__5)
    return inline641
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline665 int32 = 0
    var inline666 *ref_int32_x = ref__Ref_5int32(inline665)
    total__5 = inline666
    var for_iter413 FnIterator__int32
    var inline663 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter413 = inline663
    Loop_loop482:
    for {
        var for_next414 Option__int32
        var inline658 func() Option__int32 = for_iter413.next_fn
        var inline659 Option__int32 = inline658()
        for_next414 = inline659
        switch for_next414.(type) {
        case None:
            break Loop_loop482
        case Some:
            var x415 int32 = for_next414.(Some)._0
            var t484 int32
            var inline656 int32 = ref_get__Ref_5int32(total__5)
            t484 = inline656
            var t485 int32 = t484 + x415
            ref_set__Ref_5int32(total__5, t485)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline661 int32 = ref_get__Ref_5int32(total__5)
    return inline661
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline679 int32 = 0
    var inline680 *ref_int32_x = ref__Ref_5int32(inline679)
    total__5 = inline680
    var for_iter413 FnIterator__int32
    var inline677 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter413 = inline677
    Loop_loop490:
    for {
        var for_next414 Option__int32
        var inline672 func() Option__int32 = for_iter413.next_fn
        var inline673 Option__int32 = inline672()
        for_next414 = inline673
        switch for_next414.(type) {
        case None:
            break Loop_loop490
        case Some:
            var x415 int32 = for_next414.(Some)._0
            var t492 int32
            var inline670 int32 = ref_get__Ref_5int32(total__5)
            t492 = inline670
            var t493 int32 = t492 + x415
            ref_set__Ref_5int32(total__5, t493)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline675 int32 = ref_get__Ref_5int32(total__5)
    return inline675
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t505 string = _goml_runtime_core_int32_to_string(self__154)
    return t505
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__414 []int32) FnIterator__int32 {
    var index__415 *ref_int_x = ref__Ref_3int(0)
    var len__416 int
    var inline696 int = len(self__414)
    len__416 = inline696
    var t518 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__415,
        len_1: len__416,
        self_2: self__414,
    }
    var t519 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t518)
    }
    var inline694 FnIterator__int32 = FnIterator__int32{
        next_fn: t519,
    }
    return inline694
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env426 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__315 *ref_int_x = env426.index_0
    var len__316 int = env426.len_1
    var self__314 *_goml_vec_int32 = env426.self_2
    var current__317 int = ref_get__Ref_3int(index__315)
    var t543 bool = current__317 < len__316
    if t543 {
        var value__318 int32 = vec_get__Vec_5int32(self__314, current__317)
        var t544 int = current__317 + 1
        ref_set__Ref_3int(index__315, t544)
        var t545 Option__int32 = Some{
            _0: value__318,
        }
        return t545
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env427 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__415 *ref_int_x = env427.index_0
    var len__416 int = env427.len_1
    var self__414 []int32 = env427.self_2
    var current__417 int = ref_get__Ref_3int(index__415)
    var t550 bool = current__417 < len__416
    if t550 {
        var value__418 int32
        var inline698 int32 = self__414[current__417]
        value__418 = inline698
        var t551 int = current__417 + 1
        ref_set__Ref_3int(index__415, t551)
        var t552 Option__int32 = Some{
            _0: value__418,
        }
        return t552
    } else {
        return None{}
    }
}

func main() {
    main0()
}
