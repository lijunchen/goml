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
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int32_x
    len_1 int32
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
    var retv82 FnIterator__int32
    var t83 *ref_int32_x = self__0.conversions
    var t84 *ref_int32_x = self__0.conversions
    var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t84)
    var t86 int32 = t85 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t83, t86)
    var t87 *_goml_vec_int32 = self__0.values
    var t88 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t87)
    retv82 = t88
    return retv82
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv90 Numbers
    var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t92 int32 = t91 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t92)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t93 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv90 = t93
    return retv90
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t95 Numbers = make_numbers(builds__7, conversions__8)
    var t96 int32 = sum__S_Numbers(t95)
    println__T_int32(t96)
    var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t97)
    var t98 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t98)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t99 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t99)
    var t100 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t101 int32 = _goml_m_sum____S__Slice_l_int32_r_(t100)
    println__T_int32(t101)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv103 int32
    var t104 int32 = ref_get__Ref_5int32(self__205)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__175 *_goml_vec_int32) FnIterator__int32 {
    var retv108 FnIterator__int32
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__175)
    var t109 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t110 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t109)
    })
    retv108 = t110
    return retv108
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv112 *_goml_vec_int32
    var t113 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv117 *ref_int32_x
    var t118 *ref_int32_x = ref__Ref_5int32(value__204)
    retv117 = t118
    return retv117
}

func println__T_int32(value__1 int32) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv123 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter66 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop126:
    for {
        if true {
            var for_next67 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter66)
            switch for_next67.(type) {
            case None:
                break Loop_loop126
            case Some:
                var x68 int32 = for_next67.(Some)._0
                var value__6 int32 = x68
                var t128 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t129 int32 = t128 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t129)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop126
        }
    }
    var t125 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv123 = t125
    return retv123
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv131 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter66 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop134:
    for {
        if true {
            var for_next67 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter66)
            switch for_next67.(type) {
            case None:
                break Loop_loop134
            case Some:
                var x68 int32 = for_next67.(Some)._0
                var value__6 int32 = x68
                var t136 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t137 int32 = t136 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t137)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop134
        }
    }
    var t133 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv131 = t133
    return retv131
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv139 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter66 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop142:
    for {
        if true {
            var for_next67 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter66)
            switch for_next67.(type) {
            case None:
                break Loop_loop142
            case Some:
                var x68 int32 = for_next67.(Some)._0
                var value__6 int32 = x68
                var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t145 int32 = t144 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t145)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop142
        }
    }
    var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv139 = t141
    return retv139
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__172 *_goml_vec_int32, start__173 int32, end__174 int32) []int32 {
    var retv147 []int32
    var t148 []int32 = self__172.items[start__173:end__174]
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv150 int32
    var t151 int32 = vec_len__Vec_5int32(self__134)
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv153 int32
    var t154 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv156 FnIterator__int32
    var t157 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv159 string
    var t160 string = _goml_runtime_core_int32_to_string(self__41)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv162 Option__int32
    var t163 func() Option__int32 = self__97.next_fn
    var t164 Option__int32 = t163()
    retv162 = t164
    return retv162
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv166 FnIterator__int32
    var t167 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180)
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__192 []int32) FnIterator__int32 {
    var retv169 FnIterator__int32
    var t170 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__192)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__187 []int32) FnIterator__int32 {
    var retv172 FnIterator__int32
    var index__188 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__189 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__187)
    var t173 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__188,
        len_1: len__189,
        self_2: self__187,
    }
    var t174 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t173)
    })
    retv172 = t174
    return retv172
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183 []int32) int32 {
    var retv176 int32
    var t177 int32 = int32(len(self__183))
    retv176 = t177
    return retv176
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__181 []int32, index__182 int32) int32 {
    var retv179 int32
    var t180 int32 = self__181[index__182]
    retv179 = t180
    return retv179
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env79 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv188 Option__int32
    var index__176 *ref_int32_x = env79.index_0
    var len__177 int32 = env79.len_1
    var self__175 *_goml_vec_int32 = env79.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t191 bool = current__178 < len__177
    var jp190 Option__int32
    if t191 {
        var value__179 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__175, current__178)
        var t192 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t192)
        var t193 Option__int32 = Some{
            _0: value__179,
        }
        jp190 = t193
    } else {
        jp190 = None{}
    }
    retv188 = jp190
    return retv188
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env80 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv195 Option__int32
    var index__188 *ref_int32_x = env80.index_0
    var len__189 int32 = env80.len_1
    var self__187 []int32 = env80.self_2
    var current__190 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__188)
    var t198 bool = current__190 < len__189
    var jp197 Option__int32
    if t198 {
        var value__191 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__187, current__190)
        var t199 int32 = current__190 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__188, t199)
        var t200 Option__int32 = Some{
            _0: value__191,
        }
        jp197 = t200
    } else {
        jp197 = None{}
    }
    retv195 = jp197
    return retv195
}

func main() {
    main0()
}
