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
    var t172 int
    var inline216 int = vec_len__Vec_5int32(values__0)
    t172 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t172)
    _goml_runtime_core_string_println(inline213)
    var for_limit157 int = vec_len__Vec_5int32(values__0)
    var for_index158 int = 0
    Loop_loop174:
    for {
        var t175 bool = for_index158 < for_limit157
        if t175 {
            var for_item159 int32 = vec_get__Vec_5int32(values__0, for_index158)
            var t176 int = for_index158 + 1
            for_index158 = t176
            var inline210 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item159)
            _goml_runtime_core_string_println(inline210)
            continue
        } else {
            break Loop_loop174
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline237 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline237
    var inline234 int32 = 1
    vec_push__Vec_5int32(values__2, inline234)
    var inline231 int32 = 2
    vec_push__Vec_5int32(values__2, inline231)
    var inline228 int32 = 3
    vec_push__Vec_5int32(values__2, inline228)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline226 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline226
    var inline223 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline223)
    var inline220 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline220)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline218 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline218
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__171 *_goml_vec_int32, other__172 *_goml_vec_int32) struct{} {
    var len__173 int
    var inline247 int = vec_len__Vec_5int32(other__172)
    len__173 = inline247
    vec_reserve__Vec_5int32(self__171, len__173)
    var index__174 int = 0
    Loop_loop196:
    for {
        var t197 bool = index__174 < len__173
        if t197 {
            var t198 int32 = vec_get__Vec_5int32(other__172, index__174)
            vec_push__Vec_5int32(self__171, t198)
            var compound_old96 int = index__174
            var compound_value97 int = 1
            var t199 int = compound_old96 + compound_value97
            index__174 = t199
            continue
        } else {
            break Loop_loop196
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t203 string = _goml_runtime_core_int_to_string(self__40)
    return t203
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t206 string = _goml_runtime_core_int32_to_string(self__43)
    return t206
}

func main() {
    main0()
}
