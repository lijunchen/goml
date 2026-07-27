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
    var retv85 FnIterator__int32
    var t86 *ref_int32_x = self__0.conversions
    var t87 *ref_int32_x = self__0.conversions
    var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t87)
    var t89 int32 = t88 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t86, t89)
    var t90 *_goml_vec_int32 = self__0.values
    var t91 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t90)
    retv85 = t91
    return retv85
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv93 Numbers
    var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t95 int32 = t94 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t95)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t96 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv93 = t96
    return retv93
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t98 Numbers = make_numbers(builds__7, conversions__8)
    var t99 int32 = sum__S_Numbers(t98)
    println__T_int32(t99)
    var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t100)
    var t101 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t101)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t102 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t102)
    var t103 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t104 int32 = _goml_m_sum____S__Slice_l_int32_r_(t103)
    println__T_int32(t104)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv106 int32
    var t107 int32 = ref_get__Ref_5int32(self__210)
    retv106 = t107
    return retv106
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv111 FnIterator__int32
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__180)
    var t112 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t113 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t112)
    })
    retv111 = t113
    return retv111
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv115 *_goml_vec_int32
    var t116 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv120 *ref_int32_x
    var t121 *ref_int32_x = ref__Ref_5int32(value__209)
    retv120 = t121
    return retv120
}

func println__T_int32(value__1 int32) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv126 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter69 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop129:
    for {
        if true {
            var for_next70 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter69)
            switch for_next70.(type) {
            case None:
                break Loop_loop129
            case Some:
                var x71 int32 = for_next70.(Some)._0
                var value__6 int32 = x71
                var t131 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t132 int32 = t131 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t132)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop129
        }
    }
    var t128 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv126 = t128
    return retv126
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv134 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter69 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop137:
    for {
        if true {
            var for_next70 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter69)
            switch for_next70.(type) {
            case None:
                break Loop_loop137
            case Some:
                var x71 int32 = for_next70.(Some)._0
                var value__6 int32 = x71
                var t139 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t140 int32 = t139 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t140)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop137
        }
    }
    var t136 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv134 = t136
    return retv134
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv142 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter69 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop145:
    for {
        if true {
            var for_next70 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter69)
            switch for_next70.(type) {
            case None:
                break Loop_loop145
            case Some:
                var x71 int32 = for_next70.(Some)._0
                var value__6 int32 = x71
                var t147 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t148 int32 = t147 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t148)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop145
        }
    }
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv142 = t144
    return retv142
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__177 *_goml_vec_int32, start__178 int, end__179 int) []int32 {
    var retv150 []int32
    var t151 []int32 = self__177.items[start__178:end__179]
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv153 *ref_int_x
    var t154 *ref_int_x = ref__Ref_3int(value__209)
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv156 int
    var t157 int = vec_len__Vec_5int32(self__139)
    retv156 = t157
    return retv156
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv159 int
    var t160 int = ref_get__Ref_3int(self__210)
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv162 int32
    var t163 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv162 = t163
    return retv162
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv167 FnIterator__int32
    var t168 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv167 = t168
    return retv167
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv173 Option__int32
    var t174 func() Option__int32 = self__102.next_fn
    var t175 Option__int32 = t174()
    retv173 = t175
    return retv173
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__185 *_goml_vec_int32) FnIterator__int32 {
    var retv177 FnIterator__int32
    var t178 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__185)
    retv177 = t178
    return retv177
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__197 []int32) FnIterator__int32 {
    var retv180 FnIterator__int32
    var t181 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__197)
    retv180 = t181
    return retv180
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__192 []int32) FnIterator__int32 {
    var retv183 FnIterator__int32
    var index__193 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__194 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__192)
    var t184 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__193,
        len_1: len__194,
        self_2: self__192,
    }
    var t185 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t184)
    })
    retv183 = t185
    return retv183
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__188 []int32) int {
    var retv187 int
    var t188 int = len(self__188)
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__186 []int32, index__187 int) int32 {
    var retv190 int32
    var t191 int32 = self__186[index__187]
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env82 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv205 Option__int32
    var index__181 *ref_int_x = env82.index_0
    var len__182 int = env82.len_1
    var self__180 *_goml_vec_int32 = env82.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t208 bool = current__183 < len__182
    var jp207 Option__int32
    if t208 {
        var value__184 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__180, current__183)
        var t209 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t209)
        var t210 Option__int32 = Some{
            _0: value__184,
        }
        jp207 = t210
    } else {
        jp207 = None{}
    }
    retv205 = jp207
    return retv205
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env83 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv212 Option__int32
    var index__193 *ref_int_x = env83.index_0
    var len__194 int = env83.len_1
    var self__192 []int32 = env83.self_2
    var current__195 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__193)
    var t215 bool = current__195 < len__194
    var jp214 Option__int32
    if t215 {
        var value__196 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__192, current__195)
        var t216 int = current__195 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__193, t216)
        var t217 Option__int32 = Some{
            _0: value__196,
        }
        jp214 = t217
    } else {
        jp214 = None{}
    }
    retv212 = jp214
    return retv212
}

func main() {
    main0()
}
