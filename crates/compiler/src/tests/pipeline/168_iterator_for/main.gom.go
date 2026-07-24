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

type closure_env_goml_builtin_range_1 struct {
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
    var retv118 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t119 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t120 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t119)
    })
    retv118 = t120
    return retv118
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int32 {
    var retv122 FnIterator__int32
    var t123 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t124 int32 = t123 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t124)
    var t125 FnIterator__int32 = _goml_m_range(1, 5)
    retv122 = t125
    return retv122
}

func first_even(values__4 FnIterator__int32) int32 {
    var retv127 int32
    var for_iter63 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(values__4)
    Loop_loop129:
    for {
        if true {
            var for_next64 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter63)
            switch for_next64.(type) {
            case Option__int32_None:
                break Loop_loop129
            case Option__int32_Some:
                var x65 int32 = for_next64.(Option__int32_Some)._0
                var value__5 int32 = x65
                var t132 int32 = value__5 / 2
                var t133 int32 = t132 * 2
                var t134 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t133, value__5)
                if t134 {
                    retv127 = value__5
                    return retv127
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop129
        }
    }
    retv127 = -1
    return retv127
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter71 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__6)
    Loop_loop171:
    for {
        if true {
            var for_next72 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter71)
            switch for_next72.(type) {
            case Option__int32_None:
                break Loop_loop171
            case Option__int32_Some:
                var x73 int32 = for_next72.(Option__int32_Some)._0
                var value__8 int32 = x73
                var t176 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
                if t176 {
                    continue
                } else {
                    var t174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t175 int32 = t174 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t175)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop171
        }
    }
    var t137 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t137)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t138 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t138)
    var t139 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t139)
    var for_iter80 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(pairs__9)
    Loop_loop167:
    for {
        if true {
            var for_next81 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(for_iter80)
            switch for_next81.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop167
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x82 Tuple2_5int32_6string = for_next81.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x83 int32 = x82._0
                var x84 string = x82._1
                var text__11 string = x84
                var number__10 int32 = x83
                var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t170 string = t169 + text__11
                println__T_string(t170)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop167
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t141 FnIterator__int32 = counted_range(calls__12)
    var for_iter87 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t141)
    Loop_loop163:
    for {
        if true {
            var for_next88 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter87)
            switch for_next88.(type) {
            case Option__int32_None:
                break Loop_loop163
            case Option__int32_Some:
                var x89 int32 = for_next88.(Option__int32_Some)._0
                var value__14 int32 = x89
                var t165 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
                var t166 int32 = t165 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(range_sum__13, t166)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop163
        }
    }
    var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t143)
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
    println__T_int32(t144)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t145 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter94 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(t145)
    Loop_loop159:
    for {
        if true {
            var for_next95 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter94)
            switch for_next95.(type) {
            case Option__int32_None:
                break Loop_loop159
            case Option__int32_Some:
                var x96 int32 = for_next95.(Option__int32_Some)._0
                var value__16 int32 = x96
                var t161 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t162 int32 = t161 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t162)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop159
        }
    }
    var t147 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t147)
    var t148 FnIterator__int32 = countdown(4)
    var for_iter100 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t148)
    Loop_loop155:
    for {
        if true {
            var for_next101 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter100)
            switch for_next101.(type) {
            case Option__int32_None:
                break Loop_loop155
            case Option__int32_Some:
                var x102 int32 = for_next101.(Option__int32_Some)._0
                var value__17 int32 = x102
                var t158 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t158 {
                    break Loop_loop155
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop155
        }
    }
    var empty__18 FnIterator__int32 = _goml_m_range(0, 0)
    var for_iter106 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(empty__18)
    Loop_loop153:
    for {
        if true {
            var for_next107 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter106)
            switch for_next107.(type) {
            case Option__int32_None:
                break Loop_loop153
            case Option__int32_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop153
        }
    }
    var t151 FnIterator__int32 = _goml_m_range(3, 8)
    var t152 int32 = first_even(t151)
    println__T_int32(t152)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv178 *ref_int32_x
    var t179 *ref_int32_x = ref__Ref_5int32(value__204)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv181 int32
    var t182 int32 = ref_get__Ref_5int32(self__205)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv186 FnIterator__int32
    var t187 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv186 = t187
    return retv186
}

func _goml_m_range(start__212 int32, end__213 int32) FnIterator__int32 {
    var retv189 FnIterator__int32
    var t190 FnIterator__int32 = __goml_builtin_range(start__212, end__213)
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__104 FnIterator__int32) FnIterator__int32 {
    var retv192 FnIterator__int32
    retv192 = self__104
    return retv192
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv194 Option__int32
    var t195 func() Option__int32 = self__97.next_fn
    var t196 Option__int32 = t195()
    retv194 = t196
    return retv194
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv198 bool
    var t199 bool = self__61 == other__62
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv201 *_goml_vec_int32
    var t202 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv206 FnIterator__int32
    var t207 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180)
    retv206 = t207
    return retv206
}

func println__T_int32(value__1 int32) struct{} {
    var t209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv212 *_goml_vec_Tuple2_5int32_6string
    var t213 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv212 = t213
    return retv212
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__123 *_goml_vec_Tuple2_5int32_6string, elem__124 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(self__180 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv217 _goml_m_FnIterator_____o_int32_c_string_q_
    var t218 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__180)
    retv217 = t218
    return retv217
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(self__97 _goml_m_FnIterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv220 _goml_m_Option_____o_int32_c_string_q_
    var t221 func() _goml_m_Option_____o_int32_c_string_q_ = self__97.next_fn
    var t222 _goml_m_Option_____o_int32_c_string_q_ = t221()
    retv220 = t222
    return retv220
}

func println__T_string(value__1 string) struct{} {
    var t224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t224)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv227 string
    var t228 string = _goml_runtime_core_int32_to_string(self__5)
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__172 *_goml_vec_int32, start__173 int32, end__174 int32) []int32 {
    var retv230 []int32
    var t231 []int32 = self__172.items[start__173:end__174]
    retv230 = t231
    return retv230
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__192 []int32) FnIterator__int32 {
    var retv233 FnIterator__int32
    var t234 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__192)
    retv233 = t234
    return retv233
}

func __goml_builtin_range(start__208 int32, end__209 int32) FnIterator__int32 {
    var retv236 FnIterator__int32
    var current__210 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__208)
    var t237 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__210,
        end_1: end__209,
    }
    var t238 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t237)
    })
    retv236 = t238
    return retv236
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__175 *_goml_vec_int32) FnIterator__int32 {
    var retv240 FnIterator__int32
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__175)
    var t241 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t242 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t241)
    })
    retv240 = t242
    return retv240
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv244 string
    var t245 string = _goml_runtime_core_int32_to_string(self__41)
    retv244 = t245
    return retv244
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__175 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv247 _goml_m_FnIterator_____o_int32_c_string_q_
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__175)
    var t248 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t249 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t248)
    })
    retv247 = t249
    return retv247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv251 string
    retv251 = self__37
    return retv251
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__187 []int32) FnIterator__int32 {
    var retv253 FnIterator__int32
    var index__188 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__189 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__187)
    var t254 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__188,
        len_1: len__189,
        self_2: self__187,
    }
    var t255 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t254)
    })
    retv253 = t255
    return retv253
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv257 int32
    var t258 int32 = vec_len__Vec_5int32(self__134)
    retv257 = t258
    return retv257
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv260 int32
    var t261 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv260 = t261
    return retv260
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__134 *_goml_vec_Tuple2_5int32_6string) int32 {
    var retv263 int32
    var t264 int32 = vec_len__Vec_21Tuple2_5int32_6string(self__134)
    retv263 = t264
    return retv263
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__129 *_goml_vec_Tuple2_5int32_6string, index__130 int32) Tuple2_5int32_6string {
    var retv266 Tuple2_5int32_6string
    var t267 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__129, index__130)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(next_fn__96 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv269 _goml_m_FnIterator_____o_int32_c_string_q_
    var t270 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_FnIterator_____o_int32_c_string_q_{
        next_fn: next_fn__96,
    }
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183 []int32) int32 {
    var retv272 int32
    var t273 int32 = int32(len(self__183))
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__181 []int32, index__182 int32) int32 {
    var retv275 int32
    var t276 int32 = self__181[index__182]
    retv275 = t276
    return retv275
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env112 closure_env_countdown_0) Option__int32 {
    var retv281 Option__int32
    var current__1 *ref_int32_x = env112.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t284 bool = value__2 > 0
    var jp283 Option__int32
    if t284 {
        var t285 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t285)
        var t286 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp283 = t286
    } else {
        jp283 = Option__int32_None{}
    }
    retv281 = jp283
    return retv281
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env113 closure_env_goml_builtin_range_1) Option__int32 {
    var retv288 Option__int32
    var current__210 *ref_int32_x = env113.current_0
    var end__209 int32 = env113.end_1
    var value__211 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__210)
    var t291 bool = value__211 < end__209
    var jp290 Option__int32
    if t291 {
        var t292 int32 = value__211 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__210, t292)
        var t293 Option__int32 = Option__int32_Some{
            _0: value__211,
        }
        jp290 = t293
    } else {
        jp290 = Option__int32_None{}
    }
    retv288 = jp290
    return retv288
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env114 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv295 Option__int32
    var index__176 *ref_int32_x = env114.index_0
    var len__177 int32 = env114.len_1
    var self__175 *_goml_vec_int32 = env114.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t298 bool = current__178 < len__177
    var jp297 Option__int32
    if t298 {
        var value__179 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__175, current__178)
        var t299 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t299)
        var t300 Option__int32 = Option__int32_Some{
            _0: value__179,
        }
        jp297 = t300
    } else {
        jp297 = Option__int32_None{}
    }
    retv295 = jp297
    return retv295
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env115 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv302 _goml_m_Option_____o_int32_c_string_q_
    var index__176 *ref_int32_x = env115.index_0
    var len__177 int32 = env115.len_1
    var self__175 *_goml_vec_Tuple2_5int32_6string = env115.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t305 bool = current__178 < len__177
    var jp304 _goml_m_Option_____o_int32_c_string_q_
    if t305 {
        var value__179 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__175, current__178)
        var t306 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t306)
        var t307 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__179,
        }
        jp304 = t307
    } else {
        jp304 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv302 = jp304
    return retv302
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env116 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv309 Option__int32
    var index__188 *ref_int32_x = env116.index_0
    var len__189 int32 = env116.len_1
    var self__187 []int32 = env116.self_2
    var current__190 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__188)
    var t312 bool = current__190 < len__189
    var jp311 Option__int32
    if t312 {
        var value__191 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__187, current__190)
        var t313 int32 = current__190 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__188, t313)
        var t314 Option__int32 = Option__int32_Some{
            _0: value__191,
        }
        jp311 = t314
    } else {
        jp311 = Option__int32_None{}
    }
    retv309 = jp311
    return retv309
}

func main() {
    main0()
}
