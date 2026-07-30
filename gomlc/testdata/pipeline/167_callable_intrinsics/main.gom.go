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
    var t120 string = to_text__0(7)
    println__T_string(t120)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t121 int32 = get__1(values__3, 1)
    println__T_int32(t121)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t122 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t122)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t123 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t123)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t124 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t124)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t125 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t125)
    var t126 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int(t126)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv131 *_goml_vec_int32
    var t132 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv131 = t132
    return retv131
}

func println__T_int32(value__1 int32) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t139 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t139)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv142 int
    var t143 int = vec_len__Vec_5int32(self__137)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__134 *_goml_vec_int32, index__135 int, elem__136 int32) struct{} {
    vec_set__Vec_5int32(self__134, index__135, elem__136)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv147 int32
    var t148 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var retv150 *_goml_vec_int32
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop152:
    for {
        var t153 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t154 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t155 bool = t153 < t154
        if t155 {
            var t156 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t157 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__128, t156)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t157)
            var t158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t159 int = t158 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t159)
            continue
        } else {
            break Loop_loop152
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    retv150 = result__130
    return retv150
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv161 string
    retv161 = self__38
    return retv161
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int32_to_string(self__43)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int_to_string(self__40)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv169 *ref_int_x
    var t170 *ref_int_x = ref__Ref_3int(value__207)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv172 int
    var t173 int = ref_get__Ref_3int(self__208)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func main() {
    main0()
}
