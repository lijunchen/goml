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

type Numbers struct {
    values *_goml_vec_int32
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int32_x
    len_1 int32
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
    var retv69 *_goml_vec_int32
    var t70 *_goml_vec_int32 = self__0.values
    retv69 = t70
    return retv69
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t72 Numbers = Numbers{
        values: values__3,
    }
    var t73 int32 = count__B_Numbers(t72)
    println__T_int32(t73)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv75 *_goml_vec_int32
    var t76 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv75 = t76
    return retv75
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv83 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t84 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter58 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t84)
    Loop_loop87:
    for {
        if true {
            var for_next59 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter58)
            switch for_next59.(type) {
            case None:
                break Loop_loop87
            case Some:
                var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t90 int32 = t89 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t90)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop87
        }
    }
    var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv83 = t86
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int32_to_string(self__38)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv95 *ref_int32_x
    var t96 *ref_int32_x = ref__Ref_5int32(value__201)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__177 *_goml_vec_int32) FnIterator__int32 {
    var retv98 FnIterator__int32
    var t99 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__177)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__94 FnIterator__int32) Option__int32 {
    var retv101 Option__int32
    var t102 func() Option__int32 = self__94.next_fn
    var t103 Option__int32 = t102()
    retv101 = t103
    return retv101
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv105 int32
    var t106 int32 = ref_get__Ref_5int32(self__202)
    retv105 = t106
    return retv105
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__172 *_goml_vec_int32) FnIterator__int32 {
    var retv110 FnIterator__int32
    var index__173 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__174 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__172)
    var t111 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__173,
        len_1: len__174,
        self_2: self__172,
    }
    var t112 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t111)
    })
    retv110 = t112
    return retv110
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv114 int32
    var t115 int32 = vec_len__Vec_5int32(self__131)
    retv114 = t115
    return retv114
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv117 int32
    var t118 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv117 = t118
    return retv117
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__93 func() Option__int32) FnIterator__int32 {
    var retv120 FnIterator__int32
    var t121 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__93,
    }
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env67 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv129 Option__int32
    var index__173 *ref_int32_x = env67.index_0
    var len__174 int32 = env67.len_1
    var self__172 *_goml_vec_int32 = env67.self_2
    var current__175 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__173)
    var t132 bool = current__175 < len__174
    var jp131 Option__int32
    if t132 {
        var value__176 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__172, current__175)
        var t133 int32 = current__175 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__173, t133)
        var t134 Option__int32 = Some{
            _0: value__176,
        }
        jp131 = t134
    } else {
        jp131 = None{}
    }
    retv129 = jp131
    return retv129
}

func main() {
    main0()
}
