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
    var t81 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int(t81)
    var for_source65 *_goml_vec_int32 = values__0
    var for_limit66 int = vec_len__Vec_5int32(for_source65)
    var for_index67 int = 0
    Loop_loop83:
    for {
        var t84 bool = for_index67 < for_limit66
        if t84 {
            var for_item68 int32 = vec_get__Vec_5int32(for_source65, for_index67)
            var t85 int = for_index67 + 1
            for_index67 = t85
            var value__1 int32 = for_item68
            println__T_int32(value__1)
            continue
        } else {
            break Loop_loop83
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
    var t90 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv93 int
    var t94 int = vec_len__Vec_5int32(self__139)
    retv93 = t94
    return retv93
}

func println__T_int32(value__1 int32) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv99 *_goml_vec_int32
    var t100 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__173 *_goml_vec_int32, other__174 *_goml_vec_int32) struct{} {
    var len__175 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__174)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__173, len__175)
    var index__176 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop105:
    for {
        var t106 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
        var t107 bool = t106 < len__175
        if t107 {
            var t108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
            var t109 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__174, t108)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__173, t109)
            var t110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
            var t111 int = t110 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__176, t111)
            continue
        } else {
            break Loop_loop105
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int_to_string(self__40)
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__43)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__142 *_goml_vec_int32, additional__143 int) struct{} {
    vec_reserve__Vec_5int32(self__142, additional__143)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv121 *ref_int_x
    var t122 *ref_int_x = ref__Ref_3int(value__209)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv124 int
    var t125 int = ref_get__Ref_3int(self__210)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv127 int32
    var t128 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func main() {
    main0()
}
