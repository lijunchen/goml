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
    var retv33 *_goml_vec_int32
    var t34 *_goml_vec_int32 = self__0.values
    retv33 = t34
    return retv33
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t36 Numbers = Numbers{
        values: values__3,
    }
    var t37 int32 = count__B_Numbers(t36)
    println__T_int32(t37)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv39 *_goml_vec_int32
    var t40 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__94 *_goml_vec_int32, elem__95 int32) struct{} {
    vec_push__Vec_5int32(self__94, elem__95)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t44)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv47 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t48 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter22 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t48)
    Loop_loop51:
    for {
        if true {
            var for_next23 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter22)
            switch for_next23.(type) {
            case None:
                break Loop_loop51
            case Some:
                var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t54 int32 = t53 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t54)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop51
        }
    }
    var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv47 = t50
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int32_to_string(self__13)
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv59 *ref_int32_x
    var t60 *ref_int32_x = ref__Ref_5int32(value__137)
    retv59 = t60
    return retv59
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__114 *_goml_vec_int32) FnIterator__int32 {
    var retv62 FnIterator__int32
    var t63 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__114)
    retv62 = t63
    return retv62
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__69 FnIterator__int32) Option__int32 {
    var retv65 Option__int32
    var t66 func() Option__int32 = self__69.next_fn
    var t67 Option__int32 = t66()
    retv65 = t67
    return retv65
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv69 int32
    var t70 int32 = ref_get__Ref_5int32(self__138)
    retv69 = t70
    return retv69
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__109 *_goml_vec_int32) FnIterator__int32 {
    var retv74 FnIterator__int32
    var index__110 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__111 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__109)
    var t75 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__110,
        len_1: len__111,
        self_2: self__109,
    }
    var t76 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t75)
    })
    retv74 = t76
    return retv74
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__105 *_goml_vec_int32) int32 {
    var retv78 int32
    var t79 int32 = vec_len__Vec_5int32(self__105)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__100 *_goml_vec_int32, index__101 int32) int32 {
    var retv81 int32
    var t82 int32 = vec_get__Vec_5int32(self__100, index__101)
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__68 func() Option__int32) FnIterator__int32 {
    var retv84 FnIterator__int32
    var t85 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__68,
    }
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env31 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv93 Option__int32
    var index__110 *ref_int32_x = env31.index_0
    var len__111 int32 = env31.len_1
    var self__109 *_goml_vec_int32 = env31.self_2
    var current__112 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__110)
    var t96 bool = current__112 < len__111
    var jp95 Option__int32
    if t96 {
        var value__113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__109, current__112)
        var t97 int32 = current__112 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__110, t97)
        var t98 Option__int32 = Some{
            _0: value__113,
        }
        jp95 = t98
    } else {
        jp95 = None{}
    }
    retv93 = jp95
    return retv93
}

func main() {
    main0()
}
