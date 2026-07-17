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
    var retv72 *_goml_vec_int32
    var t73 *_goml_vec_int32 = self__0.values
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t75 Numbers = Numbers{
        values: values__3,
    }
    var t76 int32 = count__B_Numbers(t75)
    println__T_int32(t76)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv78 *_goml_vec_int32
    var t79 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t83)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv86 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t87 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter61 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t87)
    Loop_loop90:
    for {
        if true {
            var for_next62 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter61)
            switch for_next62.(type) {
            case None:
                break Loop_loop90
            case Some:
                var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t93 int32 = t92 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t93)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop90
        }
    }
    var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv86 = t89
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int32_to_string(self__41)
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv98 *ref_int32_x
    var t99 *ref_int32_x = ref__Ref_5int32(value__204)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv101 FnIterator__int32
    var t102 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv104 Option__int32
    var t105 func() Option__int32 = self__97.next_fn
    var t106 Option__int32 = t105()
    retv104 = t106
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv108 int32
    var t109 int32 = ref_get__Ref_5int32(self__205)
    retv108 = t109
    return retv108
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__175 *_goml_vec_int32) FnIterator__int32 {
    var retv113 FnIterator__int32
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__175)
    var t114 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t115 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t114)
    })
    retv113 = t115
    return retv113
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv117 int32
    var t118 int32 = vec_len__Vec_5int32(self__134)
    retv117 = t118
    return retv117
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv120 int32
    var t121 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv123 FnIterator__int32
    var t124 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env70 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv132 Option__int32
    var index__176 *ref_int32_x = env70.index_0
    var len__177 int32 = env70.len_1
    var self__175 *_goml_vec_int32 = env70.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t135 bool = current__178 < len__177
    var jp134 Option__int32
    if t135 {
        var value__179 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__175, current__178)
        var t136 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t136)
        var t137 Option__int32 = Some{
            _0: value__179,
        }
        jp134 = t137
    } else {
        jp134 = None{}
    }
    retv132 = jp134
    return retv132
}

func main() {
    main0()
}
