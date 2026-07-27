package main

import (
    _goml_fmt "fmt"
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

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t76 string = to_text__0(7)
    println__T_string(t76)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t77 int32 = get__1(values__3, 1)
    println__T_int32(t77)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t78 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t78)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t79 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t79)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t80 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t80)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t81 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t81)
    var t82 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int(t82)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv87 *_goml_vec_int32
    var t88 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv87 = t88
    return retv87
}

func println__T_int32(value__1 int32) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv98 int
    var t99 int = vec_len__Vec_5int32(self__139)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__136 *_goml_vec_int32, index__137 int, elem__138 int32) struct{} {
    vec_set__Vec_5int32(self__136, index__137, elem__138)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv103 int32
    var t104 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__130 *_goml_vec_int32, elem__131 int32) *_goml_vec_int32 {
    var retv106 *_goml_vec_int32
    var result__132 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop108:
    for {
        var t109 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t110 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__130)
        var t111 bool = t109 < t110
        if t111 {
            var t112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__130, t112)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__132, t113)
            var t114 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t115 int = t114 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t115)
            continue
        } else {
            break Loop_loop108
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__132, elem__131)
    retv106 = result__132
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv117 string
    retv117 = self__38
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int32_to_string(self__43)
    retv119 = t120
    return retv119
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int_to_string(self__40)
    retv122 = t123
    return retv122
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv125 *ref_int_x
    var t126 *ref_int_x = ref__Ref_3int(value__209)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv128 int
    var t129 int = ref_get__Ref_3int(self__210)
    retv128 = t129
    return retv128
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func main() {
    main0()
}
