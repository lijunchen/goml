package main

import (
    _goml_fmt "fmt"
)

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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int32, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t73 string = to_text__0(7)
    println__T_string(t73)
    var get__1 func(*_goml_vec_int32, int32) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t74 int32 = get__1(values__3, 1)
    println__T_int32(t74)
    var make_slice__4 func(*_goml_vec_int32, int32, int32) []int32 = func(p0 *_goml_vec_int32, p1 int32, p2 int32) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int32) int32 = func(p0 []int32, p1 int32) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t75 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t75)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t76 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t76)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t77 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t77)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t78 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t78)
    var t79 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int32(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv84 *_goml_vec_int32
    var t85 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv84 = t85
    return retv84
}

func println__T_int32(value__1 int32) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv92 int32
    var t93 int32 = vec_len__Vec_5int32(self__134)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__131 *_goml_vec_int32, index__132 int32, elem__133 int32) struct{} {
    vec_set__Vec_5int32(self__131, index__132, elem__133)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv97 int32
    var t98 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv97 = t98
    return retv97
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__125 *_goml_vec_int32, elem__126 int32) *_goml_vec_int32 {
    var retv100 *_goml_vec_int32
    var result__127 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop102:
    for {
        var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t104 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__125)
        var t105 bool = t103 < t104
        if t105 {
            var t106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t107 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__125, t106)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__127, t107)
            var t108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t109 int32 = t108 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t109)
            continue
        } else {
            break Loop_loop102
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__127, elem__126)
    retv100 = result__127
    return retv100
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv111 string
    retv111 = self__37
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int32_to_string(self__41)
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv116 *ref_int32_x
    var t117 *ref_int32_x = ref__Ref_5int32(value__204)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv119 int32
    var t120 int32 = ref_get__Ref_5int32(self__205)
    retv119 = t120
    return retv119
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func main() {
    main0()
}
