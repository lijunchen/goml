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

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: make([]int32, 0, capacity),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type Ordering int32

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t423 string = to_text__0(7)
    var inline511 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline511)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline509 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline509
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t424 int32 = get__1(values__3, 1)
    var inline506 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t424)
    _goml_runtime_core_string_println(inline506)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t425 int32 = slice_get_value__5(view__6, 0)
    var inline503 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t425)
    _goml_runtime_core_string_println(inline503)
    var inline500 int32 = 33
    vec_push__Vec_5int32(values__3, inline500)
    var t426 int
    var inline498 int = vec_len__Vec_5int32(values__3)
    t426 = inline498
    var inline495 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t426)
    _goml_runtime_core_string_println(inline495)
    var inline491 int = 0
    var inline492 int32 = 44
    vec_set__Vec_5int32(values__3, inline491, inline492)
    var t427 int32
    var inline488 int = 0
    var inline489 int32 = vec_get__Vec_5int32(values__3, inline488)
    t427 = inline489
    var inline485 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t427)
    _goml_runtime_core_string_println(inline485)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t428 int
    var inline483 int = vec_len__Vec_5int32(values__3)
    t428 = inline483
    var inline480 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t428)
    _goml_runtime_core_string_println(inline480)
    var t429 int
    var inline478 int = vec_len__Vec_5int32(copied__8)
    t429 = inline478
    var inline475 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t429)
    _goml_runtime_core_string_println(inline475)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__260 *_goml_vec_int32, elem__261 int32) *_goml_vec_int32 {
    var t454 int
    var inline527 int = vec_len__Vec_5int32(self__260)
    t454 = inline527
    var t455 int = t454 + 1
    var result__262 *_goml_vec_int32
    var inline525 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t455)
    result__262 = inline525
    var index__263 int = 0
    Loop_loop457:
    for {
        var t458 int
        var inline521 int = vec_len__Vec_5int32(self__260)
        t458 = inline521
        var t459 bool = index__263 < t458
        if t459 {
            var t460 int32 = vec_get__Vec_5int32(self__260, index__263)
            vec_push__Vec_5int32(result__262, t460)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t461 int = compound_old190 + compound_value191
            index__263 = t461
            continue
        } else {
            break Loop_loop457
        }
    }
    vec_push__Vec_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t467 string = _goml_runtime_core_int32_to_string(self__154)
    return t467
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t470 string = _goml_runtime_core_int_to_string(self__151)
    return t470
}

func main() {
    main0()
}
