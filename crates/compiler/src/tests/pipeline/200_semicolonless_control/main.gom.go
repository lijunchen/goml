package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_0 struct {
    index_0 *ref_int_x
    len_1 int
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
    var mtmp65 int = 1
    switch mtmp65 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop84:
    for {
        var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
        var t86 bool = t85 < 2
        if t86 {
            var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t88 string = _goml_m_inherent_i_int_i_int_i_to__string(t87)
            _goml_runtime_core_string_println(t88)
            var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t90 int = t89 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__0, t90)
            continue
        } else {
            break Loop_loop84
        }
    }
    var values__1 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__1, "for")
    var for_iter70 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(values__1)
    Loop_loop81:
    for {
        if true {
            var for_next71 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter70)
            switch for_next71.(type) {
            case None:
                break Loop_loop81
            case Some:
                var x72 string = for_next71.(Some)._0
                var value__2 string = x72
                _goml_runtime_core_string_println(value__2)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop81
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv96 *ref_int_x
    var t97 *ref_int_x = ref__Ref_3int(value__209)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv99 int
    var t100 int = ref_get__Ref_3int(self__210)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int_to_string(self__5)
    retv102 = t103
    return retv102
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv107 *_goml_vec_string
    var t108 *_goml_vec_string = vec_new__Vec_6string()
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__185 *_goml_vec_string) FnIterator__string {
    var retv112 FnIterator__string
    var t113 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__185)
    retv112 = t113
    return retv112
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__102 FnIterator__string) Option__string {
    var retv115 Option__string
    var t116 func() Option__string = self__102.next_fn
    var t117 Option__string = t116()
    retv115 = t117
    return retv115
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__180 *_goml_vec_string) FnIterator__string {
    var retv119 FnIterator__string
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__180)
    var t120 closure_env_inherent_Vec_Vec_T_iter_T_string_0 = closure_env_inherent_Vec_Vec_T_iter_T_string_0{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t121 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h79c10c0b54a559d578b6dccc3acba234_ring__0_i_apply(t120)
    })
    retv119 = t121
    return retv119
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__139 *_goml_vec_string) int {
    var retv123 int
    var t124 int = vec_len__Vec_6string(self__139)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__134 *_goml_vec_string, index__135 int) string {
    var retv126 string
    var t127 string = vec_get__Vec_6string(self__134, index__135)
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__101 func() Option__string) FnIterator__string {
    var retv129 FnIterator__string
    var t130 FnIterator__string = FnIterator__string{
        next_fn: next_fn__101,
    }
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_closure__en_h79c10c0b54a559d578b6dccc3acba234_ring__0_i_apply(env74 closure_env_inherent_Vec_Vec_T_iter_T_string_0) Option__string {
    var retv138 Option__string
    var index__181 *ref_int_x = env74.index_0
    var len__182 int = env74.len_1
    var self__180 *_goml_vec_string = env74.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t141 bool = current__183 < len__182
    var jp140 Option__string
    if t141 {
        var value__184 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__180, current__183)
        var t142 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t142)
        var t143 Option__string = Some{
            _0: value__184,
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
