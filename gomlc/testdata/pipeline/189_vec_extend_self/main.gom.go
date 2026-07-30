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

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t125 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int(t125)
    var for_source109 *_goml_vec_int32 = values__0
    var for_limit110 int = vec_len__Vec_5int32(for_source109)
    var for_index111 int = 0
    Loop_loop127:
    for {
        var t128 bool = for_index111 < for_limit110
        if t128 {
            var for_item112 int32 = vec_get__Vec_5int32(for_source109, for_index111)
            var t129 int = for_index111 + 1
            for_index111 = t129
            var value__1 int32 = for_item112
            println__T_int32(value__1)
            continue
        } else {
            break Loop_loop127
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
    var same__4 *_goml_vec_int32 = aliased__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, same__4)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv137 int
    var t138 int = vec_len__Vec_5int32(self__137)
    retv137 = t138
    return retv137
}

func println__T_int32(value__1 int32) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv143 *_goml_vec_int32
    var t144 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__171 *_goml_vec_int32, other__172 *_goml_vec_int32) struct{} {
    var len__173 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__172)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__171, len__173)
    var index__174 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop149:
    for {
        var t150 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
        var t151 bool = t150 < len__173
        if t151 {
            var t152 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
            var t153 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__172, t152)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__171, t153)
            var t154 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
            var t155 int = t154 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__174, t155)
            continue
        } else {
            break Loop_loop149
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv157 string
    var t158 string = _goml_runtime_core_int_to_string(self__40)
    retv157 = t158
    return retv157
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv160 string
    var t161 string = _goml_runtime_core_int32_to_string(self__43)
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__140 *_goml_vec_int32, additional__141 int) struct{} {
    vec_reserve__Vec_5int32(self__140, additional__141)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv165 *ref_int_x
    var t166 *ref_int_x = ref__Ref_3int(value__207)
    retv165 = t166
    return retv165
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv168 int
    var t169 int = ref_get__Ref_3int(self__208)
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv171 int32
    var t172 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func main() {
    main0()
}
