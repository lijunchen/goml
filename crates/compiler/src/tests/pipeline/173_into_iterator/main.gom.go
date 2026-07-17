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
    var retv79 FnIterator__int32
    var t80 *ref_int32_x = self__0.conversions
    var t81 *ref_int32_x = self__0.conversions
    var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t81)
    var t83 int32 = t82 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t80, t83)
    var t84 *_goml_vec_int32 = self__0.values
    var t85 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t84)
    retv79 = t85
    return retv79
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv87 Numbers
    var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t89 int32 = t88 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t89)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t90 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv87 = t90
    return retv87
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t92 Numbers = make_numbers(builds__7, conversions__8)
    var t93 int32 = sum__S_Numbers(t92)
    println__T_int32(t93)
    var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t94)
    var t95 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t95)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t96 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t96)
    var t97 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t98 int32 = _goml_m_sum____S__Slice_l_int32_r_(t97)
    println__T_int32(t98)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv100 int32
    var t101 int32 = ref_get__Ref_5int32(self__201)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__171 *_goml_vec_int32) FnIterator__int32 {
    var retv105 FnIterator__int32
    var index__172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__173 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__171)
    var t106 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__172,
        len_1: len__173,
        self_2: self__171,
    }
    var t107 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t106)
    })
    retv105 = t107
    return retv105
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv109 *_goml_vec_int32
    var t110 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv114 *ref_int32_x
    var t115 *ref_int32_x = ref__Ref_5int32(value__200)
    retv114 = t115
    return retv114
}

func println__T_int32(value__1 int32) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv120 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter63 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop123:
    for {
        if true {
            var for_next64 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter63)
            switch for_next64.(type) {
            case None:
                break Loop_loop123
            case Some:
                var x65 int32 = for_next64.(Some)._0
                var value__6 int32 = x65
                var t125 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t126 int32 = t125 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t126)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop123
        }
    }
    var t122 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv120 = t122
    return retv120
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv128 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter63 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop131:
    for {
        if true {
            var for_next64 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter63)
            switch for_next64.(type) {
            case None:
                break Loop_loop131
            case Some:
                var x65 int32 = for_next64.(Some)._0
                var value__6 int32 = x65
                var t133 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t134 int32 = t133 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t134)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop131
        }
    }
    var t130 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv128 = t130
    return retv128
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv136 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter63 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop139:
    for {
        if true {
            var for_next64 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter63)
            switch for_next64.(type) {
            case None:
                break Loop_loop139
            case Some:
                var x65 int32 = for_next64.(Some)._0
                var value__6 int32 = x65
                var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t142 int32 = t141 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t142)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop139
        }
    }
    var t138 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv136 = t138
    return retv136
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__168 *_goml_vec_int32, start__169 int32, end__170 int32) []int32 {
    var retv144 []int32
    var t145 []int32 = self__168.items[start__169:end__170]
    retv144 = t145
    return retv144
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv147 int32
    var t148 int32 = vec_len__Vec_5int32(self__131)
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv150 int32
    var t151 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__93 func() Option__int32) FnIterator__int32 {
    var retv153 FnIterator__int32
    var t154 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__93,
    }
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv156 string
    var t157 string = _goml_runtime_core_int32_to_string(self__38)
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__94 FnIterator__int32) Option__int32 {
    var retv159 Option__int32
    var t160 func() Option__int32 = self__94.next_fn
    var t161 Option__int32 = t160()
    retv159 = t161
    return retv159
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__176 *_goml_vec_int32) FnIterator__int32 {
    var retv163 FnIterator__int32
    var t164 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__176)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__188 []int32) FnIterator__int32 {
    var retv166 FnIterator__int32
    var t167 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__188)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__183 []int32) FnIterator__int32 {
    var retv169 FnIterator__int32
    var index__184 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__185 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183)
    var t170 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__184,
        len_1: len__185,
        self_2: self__183,
    }
    var t171 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t170)
    })
    retv169 = t171
    return retv169
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__179 []int32) int32 {
    var retv173 int32
    var t174 int32 = int32(len(self__179))
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__177 []int32, index__178 int32) int32 {
    var retv176 int32
    var t177 int32 = self__177[index__178]
    retv176 = t177
    return retv176
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env76 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv185 Option__int32
    var index__172 *ref_int32_x = env76.index_0
    var len__173 int32 = env76.len_1
    var self__171 *_goml_vec_int32 = env76.self_2
    var current__174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__172)
    var t188 bool = current__174 < len__173
    var jp187 Option__int32
    if t188 {
        var value__175 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__171, current__174)
        var t189 int32 = current__174 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__172, t189)
        var t190 Option__int32 = Some{
            _0: value__175,
        }
        jp187 = t190
    } else {
        jp187 = None{}
    }
    retv185 = jp187
    return retv185
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env77 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv192 Option__int32
    var index__184 *ref_int32_x = env77.index_0
    var len__185 int32 = env77.len_1
    var self__183 []int32 = env77.self_2
    var current__186 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__184)
    var t195 bool = current__186 < len__185
    var jp194 Option__int32
    if t195 {
        var value__187 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__183, current__186)
        var t196 int32 = current__186 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__184, t196)
        var t197 Option__int32 = Some{
            _0: value__187,
        }
        jp194 = t197
    } else {
        jp194 = None{}
    }
    retv192 = jp194
    return retv192
}

func main() {
    main0()
}
