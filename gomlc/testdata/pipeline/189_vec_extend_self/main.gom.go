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
    var t172 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int(t172)
    var for_limit157 int = vec_len__Vec_5int32(values__0)
    var for_index158 int = 0
    Loop_loop174:
    for {
        var t175 bool = for_index158 < for_limit157
        if t175 {
            var for_item159 int32 = vec_get__Vec_5int32(values__0, for_index158)
            var t176 int = for_index158 + 1
            for_index158 = t176
            println__T_int32(for_item159)
            continue
        } else {
            break Loop_loop174
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(aliased__3, 4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(aliased__3, 5)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var t185 int = vec_len__Vec_5int32(self__137)
    return t185
}

func println__T_int32(value__1 int32) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t191 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t191
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__171 *_goml_vec_int32, other__172 *_goml_vec_int32) struct{} {
    var len__173 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__172)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__171, len__173)
    var index__174 int = 0
    Loop_loop196:
    for {
        var t197 bool = index__174 < len__173
        if t197 {
            var t198 int32 = vec_get__Vec_5int32(other__172, index__174)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__171, t198)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__140 *_goml_vec_int32, additional__141 int) struct{} {
    vec_reserve__Vec_5int32(self__140, additional__141)
    return struct{}{}
}

func main() {
    main0()
}
