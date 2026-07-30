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
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 []int32
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

func _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(self__0 Numbers) FnIterator__int32 {
    var retv89 FnIterator__int32
    var t90 *ref_int32_x = self__0.conversions
    var t91 *ref_int32_x = self__0.conversions
    var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t91)
    var t93 int32 = t92 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t90, t93)
    var t94 *_goml_vec_int32 = self__0.values
    var t95 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t94)
    retv89 = t95
    return retv89
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv97 Numbers
    var t98 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t99 int32 = t98 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t99)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t100 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv97 = t100
    return retv97
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t102 Numbers = make_numbers(builds__7, conversions__8)
    var t103 int32 = sum__S_Numbers(t102)
    println__T_int32(t103)
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t104)
    var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t105)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t106 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t106)
    var t107 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t108 int32 = _goml_m_sum____S__Slice_l_int32_r_(t107)
    println__T_int32(t108)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv110 int32
    var t111 int32 = ref_get__Ref_5int32(self__208)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var retv115 FnIterator__int32
    var index__179 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t116 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t117 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t116)
    })
    retv115 = t117
    return retv115
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv119 *_goml_vec_int32
    var t120 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv119 = t120
    return retv119
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv124 *ref_int32_x
    var t125 *ref_int32_x = ref__Ref_5int32(value__207)
    retv124 = t125
    return retv124
}

func println__T_int32(value__1 int32) struct{} {
    var t127 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t127)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv130 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter73 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop133:
    for {
        if true {
            var for_next74 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter73)
            switch for_next74.(type) {
            case None:
                break Loop_loop133
            case Some:
                var x75 int32 = for_next74.(Some)._0
                var value__6 int32 = x75
                var t135 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t136 int32 = t135 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t136)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop133
        }
    }
    var t132 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv130 = t132
    return retv130
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv138 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter73 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop141:
    for {
        if true {
            var for_next74 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter73)
            switch for_next74.(type) {
            case None:
                break Loop_loop141
            case Some:
                var x75 int32 = for_next74.(Some)._0
                var value__6 int32 = x75
                var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t144 int32 = t143 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t144)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop141
        }
    }
    var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv138 = t140
    return retv138
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv146 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter73 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop149:
    for {
        if true {
            var for_next74 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter73)
            switch for_next74.(type) {
            case None:
                break Loop_loop149
            case Some:
                var x75 int32 = for_next74.(Some)._0
                var value__6 int32 = x75
                var t151 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t152 int32 = t151 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t152)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop149
        }
    }
    var t148 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv146 = t148
    return retv146
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv154 []int32
    var t155 []int32 = self__175.items[start__176:end__177]
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv157 *ref_int_x
    var t158 *ref_int_x = ref__Ref_3int(value__207)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv160 int
    var t161 int = vec_len__Vec_5int32(self__137)
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv163 int
    var t164 int = ref_get__Ref_3int(self__208)
    retv163 = t164
    return retv163
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv166 int32
    var t167 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv171 FnIterator__int32
    var t172 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv174 string
    var t175 string = _goml_runtime_core_int32_to_string(self__43)
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv177 Option__int32
    var t178 func() Option__int32 = self__102.next_fn
    var t179 Option__int32 = t178()
    retv177 = t179
    return retv177
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var retv181 FnIterator__int32
    var t182 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    retv181 = t182
    return retv181
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__195 []int32) FnIterator__int32 {
    var retv184 FnIterator__int32
    var t185 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__195)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__190 []int32) FnIterator__int32 {
    var retv187 FnIterator__int32
    var index__191 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__192 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__190)
    var t188 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__191,
        len_1: len__192,
        self_2: self__190,
    }
    var t189 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t188)
    })
    retv187 = t189
    return retv187
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv191 int
    var t192 int = len(self__186)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var retv194 int32
    var t195 int32 = self__184[index__185]
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env86 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv209 Option__int32
    var index__179 *ref_int_x = env86.index_0
    var len__180 int = env86.len_1
    var self__178 *_goml_vec_int32 = env86.self_2
    var current__181 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__179)
    var t212 bool = current__181 < len__180
    var jp211 Option__int32
    if t212 {
        var value__182 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__178, current__181)
        var t213 int = current__181 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__179, t213)
        var t214 Option__int32 = Some{
            _0: value__182,
        }
        jp211 = t214
    } else {
        jp211 = None{}
    }
    retv209 = jp211
    return retv209
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env87 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv216 Option__int32
    var index__191 *ref_int_x = env87.index_0
    var len__192 int = env87.len_1
    var self__190 []int32 = env87.self_2
    var current__193 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__191)
    var t219 bool = current__193 < len__192
    var jp218 Option__int32
    if t219 {
        var value__194 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__190, current__193)
        var t220 int = current__193 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__191, t220)
        var t221 Option__int32 = Some{
            _0: value__194,
        }
        jp218 = t221
    } else {
        jp218 = None{}
    }
    retv216 = jp218
    return retv216
}

func main() {
    main0()
}
