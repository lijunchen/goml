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
    var t153 int
    var inline197 int = vec_len__Vec_5int32(values__0)
    t153 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t153)
    _goml_runtime_core_string_println(inline194)
    var for_limit138 int = vec_len__Vec_5int32(values__0)
    var for_index139 int = 0
    Loop_loop155:
    for {
        var t156 bool = for_index139 < for_limit138
        if t156 {
            var for_item140 int32 = vec_get__Vec_5int32(values__0, for_index139)
            var t157 int = for_index139 + 1
            for_index139 = t157
            var inline191 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item140)
            _goml_runtime_core_string_println(inline191)
            continue
        } else {
            break Loop_loop155
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline218 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline218
    var inline215 int32 = 1
    vec_push__Vec_5int32(values__2, inline215)
    var inline212 int32 = 2
    vec_push__Vec_5int32(values__2, inline212)
    var inline209 int32 = 3
    vec_push__Vec_5int32(values__2, inline209)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline207 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline207
    var inline204 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline204)
    var inline201 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline201)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline199 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline199
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__196 *_goml_vec_int32, other__197 *_goml_vec_int32) struct{} {
    var len__198 int
    var inline228 int = vec_len__Vec_5int32(other__197)
    len__198 = inline228
    vec_reserve__Vec_5int32(self__196, len__198)
    var index__199 int = 0
    Loop_loop177:
    for {
        var t178 bool = index__199 < len__198
        if t178 {
            var t179 int32 = vec_get__Vec_5int32(other__197, index__199)
            vec_push__Vec_5int32(self__196, t179)
            var compound_old102 int = index__199
            var compound_value103 int = 1
            var t180 int = compound_old102 + compound_value103
            index__199 = t180
            continue
        } else {
            break Loop_loop177
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t184 string = _goml_runtime_core_int_to_string(self__69)
    return t184
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t187 string = _goml_runtime_core_int32_to_string(self__72)
    return t187
}

func main() {
    main0()
}
