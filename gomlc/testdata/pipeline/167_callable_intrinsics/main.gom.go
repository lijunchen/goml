package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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
        items: _goml_slices.Grow([]int32{}, int(capacity)),
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
    var t420 string = to_text__0(7)
    var inline508 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline508)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline506 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline506
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t421 int32 = get__1(values__3, 1)
    var inline503 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t421)
    _goml_runtime_core_string_println(inline503)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t422 int32 = slice_get_value__5(view__6, 0)
    var inline500 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t422)
    _goml_runtime_core_string_println(inline500)
    var inline497 int32 = 33
    vec_push__Vec_5int32(values__3, inline497)
    var t423 int
    var inline495 int = vec_len__Vec_5int32(values__3)
    t423 = inline495
    var inline492 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t423)
    _goml_runtime_core_string_println(inline492)
    var inline488 int = 0
    var inline489 int32 = 44
    vec_set__Vec_5int32(values__3, inline488, inline489)
    var t424 int32
    var inline485 int = 0
    var inline486 int32 = vec_get__Vec_5int32(values__3, inline485)
    t424 = inline486
    var inline482 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t424)
    _goml_runtime_core_string_println(inline482)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t425 int
    var inline480 int = vec_len__Vec_5int32(values__3)
    t425 = inline480
    var inline477 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t425)
    _goml_runtime_core_string_println(inline477)
    var t426 int
    var inline475 int = vec_len__Vec_5int32(copied__8)
    t426 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t426)
    _goml_runtime_core_string_println(inline472)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__260 *_goml_vec_int32, elem__261 int32) *_goml_vec_int32 {
    var t451 int
    var inline524 int = vec_len__Vec_5int32(self__260)
    t451 = inline524
    var t452 int = t451 + 1
    var result__262 *_goml_vec_int32
    var inline522 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t452)
    result__262 = inline522
    var index__263 int = 0
    Loop_loop454:
    for {
        var t455 int
        var inline518 int = vec_len__Vec_5int32(self__260)
        t455 = inline518
        var t456 bool = index__263 < t455
        if t456 {
            var t457 int32 = vec_get__Vec_5int32(self__260, index__263)
            vec_push__Vec_5int32(result__262, t457)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t458 int = compound_old190 + compound_value191
            index__263 = t458
            continue
        } else {
            break Loop_loop454
        }
    }
    vec_push__Vec_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t464 string = _goml_runtime_core_int32_to_string(self__154)
    return t464
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t467 string = _goml_runtime_core_int_to_string(self__151)
    return t467
}

func main() {
    main0()
}
