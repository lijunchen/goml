package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
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

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t80 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__0)
    println__T_int(t80)
    var for_iter65 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__0)
    Loop_loop82:
    for {
        if true {
            var for_next66 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter65)
            switch for_next66.(type) {
            case None:
                break Loop_loop82
            case Some:
                var x67 int32 = for_next66.(Some)._0
                var value__1 int32 = x67
                println__T_int32(value__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop82
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

func println__T_int(value__1 int) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv91 int
    var t92 int = vec_len__Vec_5int32(self__139)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__185 *_goml_vec_int32) FnIterator__int32 {
    var retv94 FnIterator__int32
    var t95 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__185)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv97 Option__int32
    var t98 func() Option__int32 = self__102.next_fn
    var t99 Option__int32 = t98()
    retv97 = t99
    return retv97
}

func println__T_int32(value__1 int32) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv104 *_goml_vec_int32
    var t105 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__173 *_goml_vec_int32, other__174 *_goml_vec_int32) struct{} {
    var len__175 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(other__174)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__173, len__175)
    var index__176 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop110:
    for {
        var t111 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
        var t112 bool = t111 < len__175
        if t112 {
            var t113 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
            var t114 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(other__174, t113)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__173, t114)
            var t115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__176)
            var t116 int = t115 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__176, t116)
            continue
        } else {
            break Loop_loop110
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int_to_string(self__40)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv121 FnIterator__int32
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__180)
    var t122 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t123 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t122)
    })
    retv121 = t123
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int32_to_string(self__43)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__142 *_goml_vec_int32, additional__143 int) struct{} {
    vec_reserve__Vec_5int32(self__142, additional__143)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv130 *ref_int_x
    var t131 *ref_int_x = ref__Ref_3int(value__209)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv133 int
    var t134 int = ref_get__Ref_3int(self__210)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv136 int32
    var t137 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv136 = t137
    return retv136
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv141 FnIterator__int32
    var t142 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv141 = t142
    return retv141
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env78 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv150 Option__int32
    var index__181 *ref_int_x = env78.index_0
    var len__182 int = env78.len_1
    var self__180 *_goml_vec_int32 = env78.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t153 bool = current__183 < len__182
    var jp152 Option__int32
    if t153 {
        var value__184 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__180, current__183)
        var t154 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t154)
        var t155 Option__int32 = Some{
            _0: value__184,
        }
        jp152 = t155
    } else {
        jp152 = None{}
    }
    retv150 = jp152
    return retv150
}

func main() {
    main0()
}
