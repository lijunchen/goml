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
    var t74 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int32(t74)
    var for_iter59 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__0)
    Loop_loop76:
    for {
        if true {
            var for_next60 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter59)
            switch for_next60.(type) {
            case None:
                break Loop_loop76
            case Some:
                var x61 int32 = for_next60.(Some)._0
                var value__1 int32 = x61
                println__T_int32(value__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop76
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
    var t82 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv85 int32
    var t86 int32 = vec_len__Vec_5int32(self__131)
    retv85 = t86
    return retv85
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__177 *_goml_vec_int32) FnIterator__int32 {
    var retv88 FnIterator__int32
    var t89 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__177)
    retv88 = t89
    return retv88
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__94 FnIterator__int32) Option__int32 {
    var retv91 Option__int32
    var t92 func() Option__int32 = self__94.next_fn
    var t93 Option__int32 = t92()
    retv91 = t93
    return retv91
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv95 *_goml_vec_int32
    var t96 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__165 *_goml_vec_int32, other__166 *_goml_vec_int32) struct{} {
    var len__167 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__166)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__165, len__167)
    var index__168 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop101:
    for {
        var t102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__168)
        var t103 bool = t102 < len__167
        if t103 {
            var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__168)
            var t105 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__166, t104)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__165, t105)
            var t106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__168)
            var t107 int32 = t106 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__168, t107)
            continue
        } else {
            break Loop_loop101
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv109 string
    var t110 string = _goml_runtime_core_int32_to_string(self__38)
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__172 *_goml_vec_int32) FnIterator__int32 {
    var retv112 FnIterator__int32
    var index__173 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__174 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__172)
    var t113 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__173,
        len_1: len__174,
        self_2: self__172,
    }
    var t114 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t113)
    })
    retv112 = t114
    return retv112
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__134 *_goml_vec_int32, additional__135 int32) struct{} {
    vec_reserve__Vec_5int32(self__134, additional__135)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv118 *ref_int32_x
    var t119 *ref_int32_x = ref__Ref_5int32(value__201)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv121 int32
    var t122 int32 = ref_get__Ref_5int32(self__202)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv124 int32
    var t125 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__93 func() Option__int32) FnIterator__int32 {
    var retv129 FnIterator__int32
    var t130 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__93,
    }
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env72 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv138 Option__int32
    var index__173 *ref_int32_x = env72.index_0
    var len__174 int32 = env72.len_1
    var self__172 *_goml_vec_int32 = env72.self_2
    var current__175 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__173)
    var t141 bool = current__175 < len__174
    var jp140 Option__int32
    if t141 {
        var value__176 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__172, current__175)
        var t142 int32 = current__175 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__173, t142)
        var t143 Option__int32 = Some{
            _0: value__176,
        }
        jp140 = t143
    } else {
        jp140 = None{}
    }
    retv138 = jp140
    return retv138
}

func main() {
    main0()
}
