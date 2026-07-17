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
    var t34 string = to_text__0(7)
    println__T_string(t34)
    var get__1 func(*_goml_vec_int32, int32) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t35 int32 = get__1(values__3, 1)
    println__T_int32(t35)
    var make_slice__4 func(*_goml_vec_int32, int32, int32) []int32 = func(p0 *_goml_vec_int32, p1 int32, p2 int32) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int32) int32 = func(p0 []int32, p1 int32) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t36 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t36)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t37 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t37)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t38 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t38)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t39 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t39)
    var t40 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int32(t40)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv45 *_goml_vec_int32
    var t46 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv45 = t46
    return retv45
}

func println__T_int32(value__1 int32) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__94 *_goml_vec_int32, elem__95 int32) struct{} {
    vec_push__Vec_5int32(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__105 *_goml_vec_int32) int32 {
    var retv53 int32
    var t54 int32 = vec_len__Vec_5int32(self__105)
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__102 *_goml_vec_int32, index__103 int32, elem__104 int32) struct{} {
    vec_set__Vec_5int32(self__102, index__103, elem__104)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__100 *_goml_vec_int32, index__101 int32) int32 {
    var retv58 int32
    var t59 int32 = vec_get__Vec_5int32(self__100, index__101)
    retv58 = t59
    return retv58
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__96 *_goml_vec_int32, elem__97 int32) *_goml_vec_int32 {
    var retv61 *_goml_vec_int32
    var result__98 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop63:
    for {
        var t64 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t65 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__96)
        var t66 bool = t64 < t65
        if t66 {
            var t67 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t68 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__96, t67)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__98, t68)
            var t69 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t70 int32 = t69 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t70)
            continue
        } else {
            break Loop_loop63
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__98, elem__97)
    retv61 = result__98
    return retv61
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv72 string
    retv72 = self__9
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__13)
    retv74 = t75
    return retv74
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv77 *ref_int32_x
    var t78 *ref_int32_x = ref__Ref_5int32(value__137)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv80 int32
    var t81 int32 = ref_get__Ref_5int32(self__138)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func main() {
    main0()
}
