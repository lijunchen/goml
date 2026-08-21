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

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var builds__7 *ref_int32_x
    var inline628 int32 = 0
    var inline629 *ref_int32_x = ref__Ref_5int32(inline628)
    builds__7 = inline629
    var conversions__8 *ref_int32_x
    var inline625 int32 = 0
    var inline626 *ref_int32_x = ref__Ref_5int32(inline625)
    conversions__8 = inline626
    var t445 Numbers
    var inline616 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    var inline617 int32 = inline616 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__7, inline617)
    var inline619 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline619, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline619, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(inline619, 3)
    var inline623 Numbers = Numbers{
        values: inline619,
        conversions: conversions__8,
    }
    t445 = inline623
    var t446 int32 = sum__S_Numbers(t445)
    var inline613 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t446)
    _goml_runtime_core_string_println(inline613)
    var t447 int32
    var inline611 int32 = ref_get__Ref_5int32(builds__7)
    t447 = inline611
    var inline608 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t447)
    _goml_runtime_core_string_println(inline608)
    var t448 int32
    var inline606 int32 = ref_get__Ref_5int32(conversions__8)
    t448 = inline606
    var inline603 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t448)
    _goml_runtime_core_string_println(inline603)
    var values__9 *_goml_vec_int32
    var inline601 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline601
    var inline598 int32 = 10
    vec_push__Vec_5int32(values__9, inline598)
    var inline595 int32 = 20
    vec_push__Vec_5int32(values__9, inline595)
    var inline592 int32 = 30
    vec_push__Vec_5int32(values__9, inline592)
    var t449 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    var inline589 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t449)
    _goml_runtime_core_string_println(inline589)
    var t450 []int32
    var inline585 int = 1
    var inline586 int = 3
    var inline587 []int32 = values__9.items[inline585:inline586]
    t450 = inline587
    var t451 int32 = _goml_m_sum____S__Slice_l_int32_r_(t450)
    var inline582 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t451)
    _goml_runtime_core_string_println(inline582)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t454 int32 = ref_get__Ref_5int32(self__432)
    return t454
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__314 *_goml_vec_int32) FnIterator__int32 {
    var index__315 *ref_int_x = ref__Ref_3int(0)
    var len__316 int
    var inline633 int = vec_len__Vec_5int32(self__314)
    len__316 = inline633
    var t459 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__315,
        len_1: len__316,
        self_2: self__314,
    }
    var t460 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t459)
    }
    var inline631 FnIterator__int32 = FnIterator__int32{
        next_fn: t460,
    }
    return inline631
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t464 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t464
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__258 *_goml_vec_int32, elem__259 int32) struct{} {
    vec_push__Vec_5int32(self__258, elem__259)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline654 int32 = 0
    var inline655 *ref_int32_x = ref__Ref_5int32(inline654)
    total__5 = inline655
    var for_iter416 FnIterator__int32
    var inline646 *ref_int32_x = source__4.conversions
    var inline647 *ref_int32_x = source__4.conversions
    var inline648 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline647)
    var inline649 int32 = inline648 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline646, inline649)
    var inline651 *_goml_vec_int32 = source__4.values
    var inline652 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(inline651)
    for_iter416 = inline652
    Loop_loop477:
    for {
        var for_next417 Option__int32
        var inline641 func() Option__int32 = for_iter416.next_fn
        var inline642 Option__int32 = inline641()
        for_next417 = inline642
        switch for_next417._tag {
        case 0:
            break Loop_loop477
        case 1:
            var x418 int32 = for_next417._v1_0
            var t479 int32
            var inline639 int32 = ref_get__Ref_5int32(total__5)
            t479 = inline639
            var t480 int32 = t479 + x418
            ref_set__Ref_5int32(total__5, t480)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline644 int32 = ref_get__Ref_5int32(total__5)
    return inline644
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline668 int32 = 0
    var inline669 *ref_int32_x = ref__Ref_5int32(inline668)
    total__5 = inline669
    var for_iter416 FnIterator__int32
    var inline666 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(source__4)
    for_iter416 = inline666
    Loop_loop485:
    for {
        var for_next417 Option__int32
        var inline661 func() Option__int32 = for_iter416.next_fn
        var inline662 Option__int32 = inline661()
        for_next417 = inline662
        switch for_next417._tag {
        case 0:
            break Loop_loop485
        case 1:
            var x418 int32 = for_next417._v1_0
            var t487 int32
            var inline659 int32 = ref_get__Ref_5int32(total__5)
            t487 = inline659
            var t488 int32 = t487 + x418
            ref_set__Ref_5int32(total__5, t488)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline664 int32 = ref_get__Ref_5int32(total__5)
    return inline664
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline682 int32 = 0
    var inline683 *ref_int32_x = ref__Ref_5int32(inline682)
    total__5 = inline683
    var for_iter416 FnIterator__int32
    var inline680 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(source__4)
    for_iter416 = inline680
    Loop_loop493:
    for {
        var for_next417 Option__int32
        var inline675 func() Option__int32 = for_iter416.next_fn
        var inline676 Option__int32 = inline675()
        for_next417 = inline676
        switch for_next417._tag {
        case 0:
            break Loop_loop493
        case 1:
            var x418 int32 = for_next417._v1_0
            var t495 int32
            var inline673 int32 = ref_get__Ref_5int32(total__5)
            t495 = inline673
            var t496 int32 = t495 + x418
            ref_set__Ref_5int32(total__5, t496)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline678 int32 = ref_get__Ref_5int32(total__5)
    return inline678
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t508 string = _goml_runtime_core_int32_to_string(self__154)
    return t508
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__414 []int32) FnIterator__int32 {
    var index__415 *ref_int_x = ref__Ref_3int(0)
    var len__416 int
    var inline699 int = len(self__414)
    len__416 = inline699
    var t521 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__415,
        len_1: len__416,
        self_2: self__414,
    }
    var t522 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t521)
    }
    var inline697 FnIterator__int32 = FnIterator__int32{
        next_fn: t522,
    }
    return inline697
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env429 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__315 *ref_int_x = env429.index_0
    var len__316 int = env429.len_1
    var self__314 *_goml_vec_int32 = env429.self_2
    var current__317 int = ref_get__Ref_3int(index__315)
    var t546 bool = current__317 < len__316
    if t546 {
        var value__318 int32 = vec_get__Vec_5int32(self__314, current__317)
        var t547 int = current__317 + 1
        ref_set__Ref_3int(index__315, t547)
        var t548 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__318,
        }
        return t548
    } else {
        return Option__int32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env430 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__415 *ref_int_x = env430.index_0
    var len__416 int = env430.len_1
    var self__414 []int32 = env430.self_2
    var current__417 int = ref_get__Ref_3int(index__415)
    var t553 bool = current__417 < len__416
    if t553 {
        var value__418 int32
        var inline701 int32 = self__414[current__417]
        value__418 = inline701
        var t554 int = current__417 + 1
        ref_set__Ref_3int(index__415, t554)
        var t555 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__418,
        }
        return t555
    } else {
        return Option__int32{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
