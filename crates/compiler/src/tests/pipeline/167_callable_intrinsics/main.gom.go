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
    var t16 string = to_text__0(7)
    println__T_string(t16)
    var get__1 func(*_goml_vec_int32, int32) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t17 int32 = get__1(values__3, 1)
    println__T_int32(t17)
    var make_slice__4 func(*_goml_vec_int32, int32, int32) []int32 = func(p0 *_goml_vec_int32, p1 int32, p2 int32) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int32) int32 = func(p0 []int32, p1 int32) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t18 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t18)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t19 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t19)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t20 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t20)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t21 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t21)
    var t22 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int32(t22)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t24 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t24)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv27 *_goml_vec_int32
    var t28 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv27 = t28
    return retv27
}

func println__T_int32(value__1 int32) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__71 *_goml_vec_int32, elem__72 int32) struct{} {
    vec_push__Vec_5int32(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__82 *_goml_vec_int32) int32 {
    var retv35 int32
    var t36 int32 = vec_len__Vec_5int32(self__82)
    retv35 = t36
    return retv35
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__79 *_goml_vec_int32, index__80 int32, elem__81 int32) struct{} {
    vec_set__Vec_5int32(self__79, index__80, elem__81)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__77 *_goml_vec_int32, index__78 int32) int32 {
    var retv40 int32
    var t41 int32 = vec_get__Vec_5int32(self__77, index__78)
    retv40 = t41
    return retv40
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__73 *_goml_vec_int32, elem__74 int32) *_goml_vec_int32 {
    var retv43 *_goml_vec_int32
    var result__75 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop45:
    for {
        var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t47 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__73)
        var t48 bool = t46 < t47
        if t48 {
            var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t50 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__73, t49)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__75, t50)
            var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t52 int32 = t51 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t52)
            continue
        } else {
            break Loop_loop45
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__75, elem__74)
    retv43 = result__75
    return retv43
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int32_to_string(self__13)
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv59 *ref_int32_x
    var t60 *ref_int32_x = ref__Ref_5int32(value__102)
    retv59 = t60
    return retv59
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv62 int32
    var t63 int32 = ref_get__Ref_5int32(self__103)
    retv62 = t63
    return retv62
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func main() {
    main0()
}
