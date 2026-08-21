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

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    if additional < 0 {
        panic("negative vector capacity")
    }
    var length int = len(vec.items)
    var required int = length + additional
    if required < length {
        panic("vector capacity overflow")
    }
    if required > cap(vec.items) {
        var next_capacity int = cap(vec.items) * 2
        if next_capacity < required {
            next_capacity = required
        }
        var next_items []int32 = make([]int32, length, next_capacity)
        copy(next_items, vec.items)
        vec.items = next_items
    }
    return struct{}{}
}

type Ordering int32

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t428 int
    var inline472 int = vec_len__Vec_5int32(values__0)
    t428 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t428)
    _goml_runtime_core_string_println(inline469)
    var for_limit413 int = vec_len__Vec_5int32(values__0)
    var for_index414 int = 0
    Loop_loop430:
    for {
        var t431 bool = for_index414 < for_limit413
        if t431 {
            var for_item415 int32 = vec_get__Vec_5int32(values__0, for_index414)
            var t432 int = for_index414 + 1
            for_index414 = t432
            var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item415)
            _goml_runtime_core_string_println(inline466)
            continue
        } else {
            break Loop_loop430
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline493 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline493
    var inline490 int32 = 1
    vec_push__Vec_5int32(values__2, inline490)
    var inline487 int32 = 2
    vec_push__Vec_5int32(values__2, inline487)
    var inline484 int32 = 3
    vec_push__Vec_5int32(values__2, inline484)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline482 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline482
    var inline479 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline479)
    var inline476 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline476)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline474 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline474
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__307 *_goml_vec_int32, other__308 *_goml_vec_int32) struct{} {
    var len__309 int
    var inline503 int = vec_len__Vec_5int32(other__308)
    len__309 = inline503
    vec_reserve__Vec_5int32(self__307, len__309)
    var index__310 int = 0
    Loop_loop452:
    for {
        var t453 bool = index__310 < len__309
        if t453 {
            var t454 int32 = vec_get__Vec_5int32(other__308, index__310)
            vec_push__Vec_5int32(self__307, t454)
            var compound_old253 int = index__310
            var compound_value254 int = 1
            var t455 int = compound_old253 + compound_value254
            index__310 = t455
            continue
        } else {
            break Loop_loop452
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t459 string = _goml_runtime_core_int_to_string(self__151)
    return t459
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t462 string = _goml_runtime_core_int32_to_string(self__154)
    return t462
}

func main() {
    main0()
}
