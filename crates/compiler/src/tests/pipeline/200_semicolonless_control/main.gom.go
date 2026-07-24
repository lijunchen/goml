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

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int32) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
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

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_0 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_string
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func main0() struct{} {
    if true {
        _goml_runtime_core_string_println("if")
    } else {
        _goml_runtime_core_string_println("else")
    }
    var mtmp62 int32 = 1
    switch mtmp62 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop81:
    for {
        var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__0)
        var t83 bool = t82 < 2
        if t83 {
            var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__0)
            var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
            _goml_runtime_core_string_println(t85)
            var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__0)
            var t87 int32 = t86 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__0, t87)
            continue
        } else {
            break Loop_loop81
        }
    }
    var values__1 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__1, "for")
    var for_iter67 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(values__1)
    Loop_loop78:
    for {
        if true {
            var for_next68 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter67)
            switch for_next68.(type) {
            case None:
                break Loop_loop78
            case Some:
                var x69 string = for_next68.(Some)._0
                var value__2 string = x69
                _goml_runtime_core_string_println(value__2)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop78
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv93 *ref_int32_x
    var t94 *ref_int32_x = ref__Ref_5int32(value__204)
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv96 int32
    var t97 int32 = ref_get__Ref_5int32(self__205)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__5)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv104 *_goml_vec_string
    var t105 *_goml_vec_string = vec_new__Vec_6string()
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__180 *_goml_vec_string) FnIterator__string {
    var retv109 FnIterator__string
    var t110 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__180)
    retv109 = t110
    return retv109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__97 FnIterator__string) Option__string {
    var retv112 Option__string
    var t113 func() Option__string = self__97.next_fn
    var t114 Option__string = t113()
    retv112 = t114
    return retv112
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__175 *_goml_vec_string) FnIterator__string {
    var retv116 FnIterator__string
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__175)
    var t117 closure_env_inherent_Vec_Vec_T_iter_T_string_0 = closure_env_inherent_Vec_Vec_T_iter_T_string_0{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t118 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h79c10c0b54a559d578b6dccc3acba234_ring__0_i_apply(t117)
    })
    retv116 = t118
    return retv116
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv120 int32
    var t121 int32 = vec_len__Vec_6string(self__134)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__129 *_goml_vec_string, index__130 int32) string {
    var retv123 string
    var t124 string = vec_get__Vec_6string(self__129, index__130)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__96 func() Option__string) FnIterator__string {
    var retv126 FnIterator__string
    var t127 FnIterator__string = FnIterator__string{
        next_fn: next_fn__96,
    }
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_closure__en_h79c10c0b54a559d578b6dccc3acba234_ring__0_i_apply(env71 closure_env_inherent_Vec_Vec_T_iter_T_string_0) Option__string {
    var retv135 Option__string
    var index__176 *ref_int32_x = env71.index_0
    var len__177 int32 = env71.len_1
    var self__175 *_goml_vec_string = env71.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t138 bool = current__178 < len__177
    var jp137 Option__string
    if t138 {
        var value__179 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__175, current__178)
        var t139 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t139)
        var t140 Option__string = Some{
            _0: value__179,
        }
        jp137 = t140
    } else {
        jp137 = None{}
    }
    retv135 = jp137
    return retv135
}

func main() {
    main0()
}
