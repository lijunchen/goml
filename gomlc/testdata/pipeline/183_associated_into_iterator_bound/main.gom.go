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
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var values__3 *_goml_vec_int32
    var inline502 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline502
    var inline499 int32 = 10
    vec_push__Vec_5int32(values__3, inline499)
    var inline496 int32 = 20
    vec_push__Vec_5int32(values__3, inline496)
    var inline493 int32 = 30
    vec_push__Vec_5int32(values__3, inline493)
    var t422 Numbers = Numbers{
        values: values__3,
    }
    var t423 int32 = count__B_Numbers(t422)
    var inline490 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t423)
    _goml_runtime_core_string_println(inline490)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline519 int32 = 0
    var inline520 *ref_int32_x = ref__Ref_5int32(inline519)
    total__2 = inline520
    var t434 *_goml_vec_int32
    var inline517 *_goml_vec_int32 = batch__1.values
    t434 = inline517
    var for_iter408 FnIterator__int32
    var inline515 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t434)
    for_iter408 = inline515
    Loop_loop437:
    for {
        var for_next409 Option__int32
        var inline510 func() Option__int32 = for_iter408.next_fn
        var inline511 Option__int32 = inline510()
        for_next409 = inline511
        switch for_next409._tag {
        case 0:
            break Loop_loop437
        case 1:
            var t439 int32
            var inline508 int32 = ref_get__Ref_5int32(total__2)
            t439 = inline508
            var t440 int32 = t439 + 1
            ref_set__Ref_5int32(total__2, t440)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline513 int32 = ref_get__Ref_5int32(total__2)
    return inline513
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__154)
    return t443
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__314 *_goml_vec_int32) FnIterator__int32 {
    var index__315 *ref_int_x = ref__Ref_3int(0)
    var len__316 int
    var inline530 int = vec_len__Vec_5int32(self__314)
    len__316 = inline530
    var t461 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__315,
        len_1: len__316,
        self_2: self__314,
    }
    var t462 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t461)
    }
    var inline528 FnIterator__int32 = FnIterator__int32{
        next_fn: t462,
    }
    return inline528
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env417 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__315 *ref_int_x = env417.index_0
    var len__316 int = env417.len_1
    var self__314 *_goml_vec_int32 = env417.self_2
    var current__317 int = ref_get__Ref_3int(index__315)
    var t486 bool = current__317 < len__316
    if t486 {
        var value__318 int32 = vec_get__Vec_5int32(self__314, current__317)
        var t487 int = current__317 + 1
        ref_set__Ref_3int(index__315, t487)
        var t488 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__318,
        }
        return t488
    } else {
        return Option__int32{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
