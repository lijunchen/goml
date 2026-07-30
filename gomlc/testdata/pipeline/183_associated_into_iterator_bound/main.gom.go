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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

type Numbers struct {
    values *_goml_vec_int32
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Batch_i_Numbers_i_items(self__0 Numbers) *_goml_vec_int32 {
    var retv119 *_goml_vec_int32
    var t120 *_goml_vec_int32 = self__0.values
    retv119 = t120
    return retv119
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t122 Numbers = Numbers{
        values: values__3,
    }
    var t123 int32 = count__B_Numbers(t122)
    println__T_int32(t123)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv125 *_goml_vec_int32
    var t126 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv133 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t134 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter108 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t134)
    Loop_loop137:
    for {
        if true {
            var for_next109 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter108)
            switch for_next109.(type) {
            case None:
                break Loop_loop137
            case Some:
                var t139 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t140 int32 = t139 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t140)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop137
        }
    }
    var t136 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv133 = t136
    return retv133
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int32_to_string(self__43)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv145 *ref_int32_x
    var t146 *ref_int32_x = ref__Ref_5int32(value__207)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var retv148 FnIterator__int32
    var t149 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv151 Option__int32
    var t152 func() Option__int32 = self__102.next_fn
    var t153 Option__int32 = t152()
    retv151 = t153
    return retv151
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv155 int32
    var t156 int32 = ref_get__Ref_5int32(self__208)
    retv155 = t156
    return retv155
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var retv160 FnIterator__int32
    var index__179 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t161 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t162 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t161)
    })
    retv160 = t162
    return retv160
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv164 *ref_int_x
    var t165 *ref_int_x = ref__Ref_3int(value__207)
    retv164 = t165
    return retv164
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv167 int
    var t168 int = vec_len__Vec_5int32(self__137)
    retv167 = t168
    return retv167
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv170 int
    var t171 int = ref_get__Ref_3int(self__208)
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv173 int32
    var t174 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv178 FnIterator__int32
    var t179 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env117 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv193 Option__int32
    var index__179 *ref_int_x = env117.index_0
    var len__180 int = env117.len_1
    var self__178 *_goml_vec_int32 = env117.self_2
    var current__181 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__179)
    var t196 bool = current__181 < len__180
    var jp195 Option__int32
    if t196 {
        var value__182 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__178, current__181)
        var t197 int = current__181 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__179, t197)
        var t198 Option__int32 = Some{
            _0: value__182,
        }
        jp195 = t198
    } else {
        jp195 = None{}
    }
    retv193 = jp195
    return retv193
}

func main() {
    main0()
}
