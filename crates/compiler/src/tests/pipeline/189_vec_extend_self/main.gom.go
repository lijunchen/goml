package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int32) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
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

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t77 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int32(t77)
    var for_iter62 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__0)
    Loop_loop79:
    for {
        if true {
            var for_next63 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter62)
            switch for_next63.(type) {
            case None:
                break Loop_loop79
            case Some:
                var x64 int32 = for_next63.(Some)._0
                var value__1 int32 = x64
                println__T_int32(value__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop79
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__2, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(aliased__3, 4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(aliased__3, 5)
    var same__4 *_goml_vec_int32 = aliased__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, same__4)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv88 int32
    var t89 int32 = vec_len__Vec_5int32(self__134)
    retv88 = t89
    return retv88
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv91 FnIterator__int32
    var t92 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv94 Option__int32
    var t95 func() Option__int32 = self__97.next_fn
    var t96 Option__int32 = t95()
    retv94 = t96
    return retv94
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv98 *_goml_vec_int32
    var t99 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__168 *_goml_vec_int32, other__169 *_goml_vec_int32) struct{} {
    var len__170 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__169)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__168, len__170)
    var index__171 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop104:
    for {
        var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__171)
        var t106 bool = t105 < len__170
        if t106 {
            var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__171)
            var t108 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__169, t107)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__168, t108)
            var t109 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__171)
            var t110 int32 = t109 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__171, t110)
            continue
        } else {
            break Loop_loop104
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv112 string
    var t113 string = _goml_runtime_core_int32_to_string(self__41)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__175 *_goml_vec_int32) FnIterator__int32 {
    var retv115 FnIterator__int32
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__175)
    var t116 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t117 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t116)
    })
    retv115 = t117
    return retv115
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__137 *_goml_vec_int32, additional__138 int32) struct{} {
    vec_reserve__Vec_5int32(self__137, additional__138)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv121 *ref_int32_x
    var t122 *ref_int32_x = ref__Ref_5int32(value__204)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv124 int32
    var t125 int32 = ref_get__Ref_5int32(self__205)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv127 int32
    var t128 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv132 FnIterator__int32
    var t133 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env75 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv141 Option__int32
    var index__176 *ref_int32_x = env75.index_0
    var len__177 int32 = env75.len_1
    var self__175 *_goml_vec_int32 = env75.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t144 bool = current__178 < len__177
    var jp143 Option__int32
    if t144 {
        var value__179 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__175, current__178)
        var t145 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t145)
        var t146 Option__int32 = Some{
            _0: value__179,
        }
        jp143 = t146
    } else {
        jp143 = None{}
    }
    retv141 = jp143
    return retv141
}

func main() {
    main0()
}
