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

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
}

type Ordering int32

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t425 int
    var inline469 int = vec_len__Vec_5int32(values__0)
    t425 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t425)
    _goml_runtime_core_string_println(inline466)
    var for_limit410 int = vec_len__Vec_5int32(values__0)
    var for_index411 int = 0
    Loop_loop427:
    for {
        var t428 bool = for_index411 < for_limit410
        if t428 {
            var for_item412 int32 = vec_get__Vec_5int32(values__0, for_index411)
            var t429 int = for_index411 + 1
            for_index411 = t429
            var inline463 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item412)
            _goml_runtime_core_string_println(inline463)
            continue
        } else {
            break Loop_loop427
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline490 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline490
    var inline487 int32 = 1
    vec_push__Vec_5int32(values__2, inline487)
    var inline484 int32 = 2
    vec_push__Vec_5int32(values__2, inline484)
    var inline481 int32 = 3
    vec_push__Vec_5int32(values__2, inline481)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline479 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline479
    var inline476 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline476)
    var inline473 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline473)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline471 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline471
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__307 *_goml_vec_int32, other__308 *_goml_vec_int32) struct{} {
    var len__309 int
    var inline500 int = vec_len__Vec_5int32(other__308)
    len__309 = inline500
    vec_reserve__Vec_5int32(self__307, len__309)
    var index__310 int = 0
    Loop_loop449:
    for {
        var t450 bool = index__310 < len__309
        if t450 {
            var t451 int32 = vec_get__Vec_5int32(other__308, index__310)
            vec_push__Vec_5int32(self__307, t451)
            var compound_old253 int = index__310
            var compound_value254 int = 1
            var t452 int = compound_old253 + compound_value254
            index__310 = t452
            continue
        } else {
            break Loop_loop449
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t456 string = _goml_runtime_core_int_to_string(self__151)
    return t456
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t459 string = _goml_runtime_core_int32_to_string(self__154)
    return t459
}

func main() {
    main0()
}
