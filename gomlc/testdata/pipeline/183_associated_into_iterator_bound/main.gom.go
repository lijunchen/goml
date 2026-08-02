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
    var retv166 *_goml_vec_int32
    var t167 *_goml_vec_int32 = self__0.values
    retv166 = t167
    return retv166
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t169 Numbers = Numbers{
        values: values__3,
    }
    var t170 int32 = count__B_Numbers(t169)
    println__T_int32(t170)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv172 *_goml_vec_int32
    var t173 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t177 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t177)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv180 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t181 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter155 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t181)
    Loop_loop184:
    for {
        if true {
            var for_next156 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter155)
            switch for_next156.(type) {
            case None:
                break Loop_loop184
            case Some:
                var t186 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t187 int32 = t186 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t187)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop184
        }
    }
    var t183 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv180 = t183
    return retv180
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv189 string
    var t190 string = _goml_runtime_core_int32_to_string(self__43)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv192 *ref_int32_x
    var t193 *ref_int32_x = ref__Ref_5int32(value__207)
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var retv195 FnIterator__int32
    var t196 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv198 Option__int32
    var t199 func() Option__int32 = self__102.next_fn
    var t200 Option__int32 = t199()
    retv198 = t200
    return retv198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv202 int32
    var t203 int32 = ref_get__Ref_5int32(self__208)
    retv202 = t203
    return retv202
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var retv207 FnIterator__int32
    var index__179 *ref_int_x = ref__Ref_3int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t208 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t209 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t208)
    })
    retv207 = t209
    return retv207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv211 int
    var t212 int = vec_len__Vec_5int32(self__137)
    retv211 = t212
    return retv211
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv214 FnIterator__int32
    var t215 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env164 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv229 Option__int32
    var index__179 *ref_int_x = env164.index_0
    var len__180 int = env164.len_1
    var self__178 *_goml_vec_int32 = env164.self_2
    var current__181 int = ref_get__Ref_3int(index__179)
    var t232 bool = current__181 < len__180
    var jp231 Option__int32
    if t232 {
        var value__182 int32 = vec_get__Vec_5int32(self__178, current__181)
        var t233 int = current__181 + 1
        ref_set__Ref_3int(index__179, t233)
        var t234 Option__int32 = Some{
            _0: value__182,
        }
        jp231 = t234
    } else {
        jp231 = None{}
    }
    retv229 = jp231
    return retv229
}

func main() {
    main0()
}
