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

type _goml_vec_Tuple2_5int32_6string struct {
    items []Tuple2_5int32_6string
}

func vec_new__Vec_21Tuple2_5int32_6string() *_goml_vec_Tuple2_5int32_6string {
    return &_goml_vec_Tuple2_5int32_6string{
        items: nil,
    }
}

func vec_push__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, elem Tuple2_5int32_6string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, index int32) Tuple2_5int32_6string {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string) int32 {
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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type _goml_m_FnIterator_____o_int32_c_string_q_ struct {
    next_fn func() _goml_m_Option_____o_int32_c_string_q_
}

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_range_1 struct {
    current_0 *ref_int32_x
    end_1 int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_2 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_Tuple2_5int32_6string
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_4 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 []int32
}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type _goml_m_Option_____o_int32_c_string_q_ interface {
    is_goml_m_Option_____o_int32_c_string_q_()
}

type _goml_m_Option_____o_int32_c_string_q__None struct {}

func (_ _goml_m_Option_____o_int32_c_string_q__None) is_goml_m_Option_____o_int32_c_string_q_() {}

type _goml_m_Option_____o_int32_c_string_q__Some struct {
    _0 Tuple2_5int32_6string
}

func (_ _goml_m_Option_____o_int32_c_string_q__Some) is_goml_m_Option_____o_int32_c_string_q_() {}

func countdown(start__0 int32) FnIterator__int32 {
    var retv115 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t116 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t117 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t116)
    })
    retv115 = t117
    return retv115
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int32 {
    var retv119 FnIterator__int32
    var t120 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t121 int32 = t120 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t121)
    var t122 FnIterator__int32 = _goml_m_range(1, 5)
    retv119 = t122
    return retv119
}

func first_even(values__4 FnIterator__int32) int32 {
    var retv124 int32
    var for_iter60 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(values__4)
    Loop_loop126:
    for {
        if true {
            var for_next61 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter60)
            switch for_next61.(type) {
            case Option__int32_None:
                break Loop_loop126
            case Option__int32_Some:
                var x62 int32 = for_next61.(Option__int32_Some)._0
                var value__5 int32 = x62
                var t129 int32 = value__5 / 2
                var t130 int32 = t129 * 2
                var t131 bool = t130 == value__5
                if t131 {
                    retv124 = value__5
                    return retv124
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop126
        }
    }
    retv124 = -1
    return retv124
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter68 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__6)
    Loop_loop168:
    for {
        if true {
            var for_next69 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter68)
            switch for_next69.(type) {
            case Option__int32_None:
                break Loop_loop168
            case Option__int32_Some:
                var x70 int32 = for_next69.(Option__int32_Some)._0
                var value__8 int32 = x70
                var t173 bool = value__8 == 20
                if t173 {
                    continue
                } else {
                    var t171 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t172 int32 = t171 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t172)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop168
        }
    }
    var t134 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t134)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t135 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t135)
    var t136 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t136)
    var for_iter77 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(pairs__9)
    Loop_loop164:
    for {
        if true {
            var for_next78 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(for_iter77)
            switch for_next78.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop164
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x79 Tuple2_5int32_6string = for_next78.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x80 int32 = x79._0
                var x81 string = x79._1
                var text__11 string = x81
                var number__10 int32 = x80
                var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t167 string = t166 + text__11
                println__T_string(t167)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop164
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t138 FnIterator__int32 = counted_range(calls__12)
    var for_iter84 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t138)
    Loop_loop160:
    for {
        if true {
            var for_next85 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter84)
            switch for_next85.(type) {
            case Option__int32_None:
                break Loop_loop160
            case Option__int32_Some:
                var x86 int32 = for_next85.(Option__int32_Some)._0
                var value__14 int32 = x86
                var t162 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
                var t163 int32 = t162 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(range_sum__13, t163)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop160
        }
    }
    var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t140)
    var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
    println__T_int32(t141)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t142 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter91 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(t142)
    Loop_loop156:
    for {
        if true {
            var for_next92 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter91)
            switch for_next92.(type) {
            case Option__int32_None:
                break Loop_loop156
            case Option__int32_Some:
                var x93 int32 = for_next92.(Option__int32_Some)._0
                var value__16 int32 = x93
                var t158 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t159 int32 = t158 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t159)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop156
        }
    }
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t144)
    var t145 FnIterator__int32 = countdown(4)
    var for_iter97 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t145)
    Loop_loop152:
    for {
        if true {
            var for_next98 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter97)
            switch for_next98.(type) {
            case Option__int32_None:
                break Loop_loop152
            case Option__int32_Some:
                var x99 int32 = for_next98.(Option__int32_Some)._0
                var value__17 int32 = x99
                var t155 bool = value__17 == 2
                if t155 {
                    break Loop_loop152
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop152
        }
    }
    var empty__18 FnIterator__int32 = _goml_m_range(0, 0)
    var for_iter103 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(empty__18)
    Loop_loop150:
    for {
        if true {
            var for_next104 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter103)
            switch for_next104.(type) {
            case Option__int32_None:
                break Loop_loop150
            case Option__int32_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop150
        }
    }
    var t148 FnIterator__int32 = _goml_m_range(3, 8)
    var t149 int32 = first_even(t148)
    println__T_int32(t149)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv175 *ref_int32_x
    var t176 *ref_int32_x = ref__Ref_5int32(value__200)
    retv175 = t176
    return retv175
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv178 int32
    var t179 int32 = ref_get__Ref_5int32(self__201)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__93 func() Option__int32) FnIterator__int32 {
    var retv183 FnIterator__int32
    var t184 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__93,
    }
    retv183 = t184
    return retv183
}

func _goml_m_range(start__204 int32, end__205 int32) FnIterator__int32 {
    var retv186 FnIterator__int32
    var current__206 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__204)
    var t187 closure_env_range_1 = closure_env_range_1{
        current_0: current__206,
        end_1: end__205,
    }
    var t188 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(t187)
    })
    retv186 = t188
    return retv186
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__101 FnIterator__int32) FnIterator__int32 {
    var retv190 FnIterator__int32
    retv190 = self__101
    return retv190
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__94 FnIterator__int32) Option__int32 {
    var retv192 Option__int32
    var t193 func() Option__int32 = self__94.next_fn
    var t194 Option__int32 = t193()
    retv192 = t194
    return retv192
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv196 *_goml_vec_int32
    var t197 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv196 = t197
    return retv196
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__176 *_goml_vec_int32) FnIterator__int32 {
    var retv201 FnIterator__int32
    var t202 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__176)
    retv201 = t202
    return retv201
}

func println__T_int32(value__1 int32) struct{} {
    var t204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv207 *_goml_vec_Tuple2_5int32_6string
    var t208 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv207 = t208
    return retv207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__120 *_goml_vec_Tuple2_5int32_6string, elem__121 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__120, elem__121)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(self__176 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv212 _goml_m_FnIterator_____o_int32_c_string_q_
    var t213 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__176)
    retv212 = t213
    return retv212
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(self__94 _goml_m_FnIterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv215 _goml_m_Option_____o_int32_c_string_q_
    var t216 func() _goml_m_Option_____o_int32_c_string_q_ = self__94.next_fn
    var t217 _goml_m_Option_____o_int32_c_string_q_ = t216()
    retv215 = t217
    return retv215
}

func println__T_string(value__1 string) struct{} {
    var t219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv222 string
    var t223 string = _goml_runtime_core_int32_to_string(self__2)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__168 *_goml_vec_int32, start__169 int32, end__170 int32) []int32 {
    var retv225 []int32
    var t226 []int32 = self__168.items[start__169:end__170]
    retv225 = t226
    return retv225
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__188 []int32) FnIterator__int32 {
    var retv228 FnIterator__int32
    var t229 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__188)
    retv228 = t229
    return retv228
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__171 *_goml_vec_int32) FnIterator__int32 {
    var retv231 FnIterator__int32
    var index__172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__173 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__171)
    var t232 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__172,
        len_1: len__173,
        self_2: self__171,
    }
    var t233 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t232)
    })
    retv231 = t233
    return retv231
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv235 string
    var t236 string = _goml_runtime_core_int32_to_string(self__38)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__171 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv238 _goml_m_FnIterator_____o_int32_c_string_q_
    var index__172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__173 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__171)
    var t239 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__172,
        len_1: len__173,
        self_2: self__171,
    }
    var t240 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t239)
    })
    retv238 = t240
    return retv238
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv242 string
    retv242 = self__34
    return retv242
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__183 []int32) FnIterator__int32 {
    var retv244 FnIterator__int32
    var index__184 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__185 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183)
    var t245 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__184,
        len_1: len__185,
        self_2: self__183,
    }
    var t246 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t245)
    })
    retv244 = t246
    return retv244
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv248 int32
    var t249 int32 = vec_len__Vec_5int32(self__131)
    retv248 = t249
    return retv248
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv251 int32
    var t252 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv251 = t252
    return retv251
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__131 *_goml_vec_Tuple2_5int32_6string) int32 {
    var retv254 int32
    var t255 int32 = vec_len__Vec_21Tuple2_5int32_6string(self__131)
    retv254 = t255
    return retv254
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__126 *_goml_vec_Tuple2_5int32_6string, index__127 int32) Tuple2_5int32_6string {
    var retv257 Tuple2_5int32_6string
    var t258 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__126, index__127)
    retv257 = t258
    return retv257
}

func _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(next_fn__93 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv260 _goml_m_FnIterator_____o_int32_c_string_q_
    var t261 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_FnIterator_____o_int32_c_string_q_{
        next_fn: next_fn__93,
    }
    retv260 = t261
    return retv260
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__179 []int32) int32 {
    var retv263 int32
    var t264 int32 = int32(len(self__179))
    retv263 = t264
    return retv263
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__177 []int32, index__178 int32) int32 {
    var retv266 int32
    var t267 int32 = self__177[index__178]
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env109 closure_env_countdown_0) Option__int32 {
    var retv275 Option__int32
    var current__1 *ref_int32_x = env109.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t278 bool = value__2 > 0
    var jp277 Option__int32
    if t278 {
        var t279 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t279)
        var t280 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp277 = t280
    } else {
        jp277 = Option__int32_None{}
    }
    retv275 = jp277
    return retv275
}

func _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(env110 closure_env_range_1) Option__int32 {
    var retv282 Option__int32
    var current__206 *ref_int32_x = env110.current_0
    var end__205 int32 = env110.end_1
    var value__207 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__206)
    var t285 bool = value__207 < end__205
    var jp284 Option__int32
    if t285 {
        var t286 int32 = value__207 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__206, t286)
        var t287 Option__int32 = Option__int32_Some{
            _0: value__207,
        }
        jp284 = t287
    } else {
        jp284 = Option__int32_None{}
    }
    retv282 = jp284
    return retv282
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env111 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv289 Option__int32
    var index__172 *ref_int32_x = env111.index_0
    var len__173 int32 = env111.len_1
    var self__171 *_goml_vec_int32 = env111.self_2
    var current__174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__172)
    var t292 bool = current__174 < len__173
    var jp291 Option__int32
    if t292 {
        var value__175 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__171, current__174)
        var t293 int32 = current__174 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__172, t293)
        var t294 Option__int32 = Option__int32_Some{
            _0: value__175,
        }
        jp291 = t294
    } else {
        jp291 = Option__int32_None{}
    }
    retv289 = jp291
    return retv289
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env112 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv296 _goml_m_Option_____o_int32_c_string_q_
    var index__172 *ref_int32_x = env112.index_0
    var len__173 int32 = env112.len_1
    var self__171 *_goml_vec_Tuple2_5int32_6string = env112.self_2
    var current__174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__172)
    var t299 bool = current__174 < len__173
    var jp298 _goml_m_Option_____o_int32_c_string_q_
    if t299 {
        var value__175 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__171, current__174)
        var t300 int32 = current__174 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__172, t300)
        var t301 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__175,
        }
        jp298 = t301
    } else {
        jp298 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv296 = jp298
    return retv296
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env113 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv303 Option__int32
    var index__184 *ref_int32_x = env113.index_0
    var len__185 int32 = env113.len_1
    var self__183 []int32 = env113.self_2
    var current__186 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__184)
    var t306 bool = current__186 < len__185
    var jp305 Option__int32
    if t306 {
        var value__187 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__183, current__186)
        var t307 int32 = current__186 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__184, t307)
        var t308 Option__int32 = Option__int32_Some{
            _0: value__187,
        }
        jp305 = t308
    } else {
        jp305 = Option__int32_None{}
    }
    retv303 = jp305
    return retv303
}

func main() {
    main0()
}
