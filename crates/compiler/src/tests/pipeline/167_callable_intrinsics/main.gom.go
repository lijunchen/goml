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
    var t19 string = to_text__0(7)
    println__T_string(t19)
    var get__1 func(*_goml_vec_int32, int32) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t20 int32 = get__1(values__3, 1)
    println__T_int32(t20)
    var make_slice__4 func(*_goml_vec_int32, int32, int32) []int32 = func(p0 *_goml_vec_int32, p1 int32, p2 int32) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int32) int32 = func(p0 []int32, p1 int32) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t21 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t21)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t22 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t22)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t23 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t23)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t24 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t24)
    var t25 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int32(t25)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t27 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t27)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv30 *_goml_vec_int32
    var t31 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv30 = t31
    return retv30
}

func println__T_int32(value__1 int32) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__73 *_goml_vec_int32, elem__74 int32) struct{} {
    vec_push__Vec_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__84 *_goml_vec_int32) int32 {
    var retv38 int32
    var t39 int32 = vec_len__Vec_5int32(self__84)
    retv38 = t39
    return retv38
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__81 *_goml_vec_int32, index__82 int32, elem__83 int32) struct{} {
    vec_set__Vec_5int32(self__81, index__82, elem__83)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__79 *_goml_vec_int32, index__80 int32) int32 {
    var retv43 int32
    var t44 int32 = vec_get__Vec_5int32(self__79, index__80)
    retv43 = t44
    return retv43
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__75 *_goml_vec_int32, elem__76 int32) *_goml_vec_int32 {
    var retv46 *_goml_vec_int32
    var result__77 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop48:
    for {
        var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t50 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__75)
        var t51 bool = t49 < t50
        if t51 {
            var t52 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t53 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__75, t52)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__77, t53)
            var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t55 int32 = t54 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t55)
            continue
        } else {
            break Loop_loop48
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__77, elem__76)
    retv46 = result__77
    return retv46
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv59 string
    var t60 string = _goml_runtime_core_int32_to_string(self__13)
    retv59 = t60
    return retv59
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv62 *ref_int32_x
    var t63 *ref_int32_x = ref__Ref_5int32(value__114)
    retv62 = t63
    return retv62
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv65 int32
    var t66 int32 = ref_get__Ref_5int32(self__115)
    retv65 = t66
    return retv65
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func main() {
    main0()
}
