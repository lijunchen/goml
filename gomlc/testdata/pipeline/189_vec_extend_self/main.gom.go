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
    var t85 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int(t85)
    var for_source69 *_goml_vec_int32 = values__0
    var for_limit70 int = vec_len__Vec_5int32(for_source69)
    var for_index71 int = 0
    Loop_loop87:
    for {
        var t88 bool = for_index71 < for_limit70
        if t88 {
            var for_item72 int32 = vec_get__Vec_5int32(for_source69, for_index71)
            var t89 int = for_index71 + 1
            for_index71 = t89
            var value__1 int32 = for_item72
            println__T_int32(value__1)
            continue
        } else {
            break Loop_loop87
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
    var t94 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv97 int
    var t98 int = vec_len__Vec_5int32(self__137)
    retv97 = t98
    return retv97
}

func println__T_int32(value__1 int32) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv103 *_goml_vec_int32
    var t104 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__171 *_goml_vec_int32, other__172 *_goml_vec_int32) struct{} {
    var len__173 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__172)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__171, len__173)
    var index__174 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop109:
    for {
        var t110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
        var t111 bool = t110 < len__173
        if t111 {
            var t112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
            var t113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__172, t112)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__171, t113)
            var t114 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__174)
            var t115 int = t114 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__174, t115)
            continue
        } else {
            break Loop_loop109
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int_to_string(self__40)
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__43)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__140 *_goml_vec_int32, additional__141 int) struct{} {
    vec_reserve__Vec_5int32(self__140, additional__141)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv125 *ref_int_x
    var t126 *ref_int_x = ref__Ref_3int(value__207)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv128 int
    var t129 int = ref_get__Ref_3int(self__208)
    retv128 = t129
    return retv128
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv131 int32
    var t132 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv131 = t132
    return retv131
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func main() {
    main0()
}
