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
    var retv75 *_goml_vec_int32
    var t76 *_goml_vec_int32 = self__0.values
    retv75 = t76
    return retv75
}

func main0() struct{} {
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 30)
    var t78 Numbers = Numbers{
        values: values__3,
    }
    var t79 int32 = count__B_Numbers(t78)
    println__T_int32(t79)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv81 *_goml_vec_int32
    var t82 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var retv89 int32
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t90 *_goml_vec_int32 = _goml_m_trait__impl_i_Batch_i_Numbers_i_items(batch__1)
    var for_iter64 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(t90)
    Loop_loop93:
    for {
        if true {
            var for_next65 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter64)
            switch for_next65.(type) {
            case None:
                break Loop_loop93
            case Some:
                var t95 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
                var t96 int32 = t95 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t96)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop93
        }
    }
    var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    retv89 = t92
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__43)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv101 *ref_int32_x
    var t102 *ref_int32_x = ref__Ref_5int32(value__209)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__185 *_goml_vec_int32) FnIterator__int32 {
    var retv104 FnIterator__int32
    var t105 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__185)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv107 Option__int32
    var t108 func() Option__int32 = self__102.next_fn
    var t109 Option__int32 = t108()
    retv107 = t109
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv111 int32
    var t112 int32 = ref_get__Ref_5int32(self__210)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv116 FnIterator__int32
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__180)
    var t117 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t118 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t117)
    })
    retv116 = t118
    return retv116
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv120 *ref_int_x
    var t121 *ref_int_x = ref__Ref_3int(value__209)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv123 int
    var t124 int = vec_len__Vec_5int32(self__139)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv126 int
    var t127 int = ref_get__Ref_3int(self__210)
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv129 int32
    var t130 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv134 FnIterator__int32
    var t135 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env73 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv149 Option__int32
    var index__181 *ref_int_x = env73.index_0
    var len__182 int = env73.len_1
    var self__180 *_goml_vec_int32 = env73.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t152 bool = current__183 < len__182
    var jp151 Option__int32
    if t152 {
        var value__184 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__180, current__183)
        var t153 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t153)
        var t154 Option__int32 = Some{
            _0: value__184,
        }
        jp151 = t154
    } else {
        jp151 = None{}
    }
    retv149 = jp151
    return retv149
}

func main() {
    main0()
}
