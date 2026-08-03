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

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t194 int
    var inline238 int = vec_len__Vec_5int32(values__0)
    t194 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t194)
    _goml_runtime_core_string_println(inline235)
    var for_limit179 int = vec_len__Vec_5int32(values__0)
    var for_index180 int = 0
    Loop_loop196:
    for {
        var t197 bool = for_index180 < for_limit179
        if t197 {
            var for_item181 int32 = vec_get__Vec_5int32(values__0, for_index180)
            var t198 int = for_index180 + 1
            for_index180 = t198
            var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item181)
            _goml_runtime_core_string_println(inline232)
            continue
        } else {
            break Loop_loop196
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline259 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline259
    var inline256 int32 = 1
    vec_push__Vec_5int32(values__2, inline256)
    var inline253 int32 = 2
    vec_push__Vec_5int32(values__2, inline253)
    var inline250 int32 = 3
    vec_push__Vec_5int32(values__2, inline250)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline248 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline248
    var inline245 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline245)
    var inline242 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline242)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline240 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline240
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__200 *_goml_vec_int32, other__201 *_goml_vec_int32) struct{} {
    var len__202 int
    var inline269 int = vec_len__Vec_5int32(other__201)
    len__202 = inline269
    vec_reserve__Vec_5int32(self__200, len__202)
    var index__203 int = 0
    Loop_loop218:
    for {
        var t219 bool = index__203 < len__202
        if t219 {
            var t220 int32 = vec_get__Vec_5int32(other__201, index__203)
            vec_push__Vec_5int32(self__200, t220)
            var compound_old118 int = index__203
            var compound_value119 int = 1
            var t221 int = compound_old118 + compound_value119
            index__203 = t221
            continue
        } else {
            break Loop_loop218
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t225 string = _goml_runtime_core_int_to_string(self__69)
    return t225
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t228 string = _goml_runtime_core_int32_to_string(self__72)
    return t228
}

func main() {
    main0()
}
