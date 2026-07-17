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
    var retv79 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t80 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t81 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t80)
    })
    retv79 = t81
    return retv79
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int32 {
    var retv83 FnIterator__int32
    var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t85 int32 = t84 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t85)
    var t86 FnIterator__int32 = _goml_m_range(1, 5)
    retv83 = t86
    return retv83
}

func first_even(values__4 FnIterator__int32) int32 {
    var retv88 int32
    var for_iter24 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(values__4)
    Loop_loop90:
    for {
        if true {
            var for_next25 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter24)
            switch for_next25.(type) {
            case Option__int32_None:
                break Loop_loop90
            case Option__int32_Some:
                var x26 int32 = for_next25.(Option__int32_Some)._0
                var value__5 int32 = x26
                var t93 int32 = value__5 / 2
                var t94 int32 = t93 * 2
                var t95 bool = t94 == value__5
                if t95 {
                    retv88 = value__5
                    return retv88
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop90
        }
    }
    retv88 = -1
    return retv88
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter32 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__6)
    Loop_loop132:
    for {
        if true {
            var for_next33 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter32)
            switch for_next33.(type) {
            case Option__int32_None:
                break Loop_loop132
            case Option__int32_Some:
                var x34 int32 = for_next33.(Option__int32_Some)._0
                var value__8 int32 = x34
                var t137 bool = value__8 == 20
                if t137 {
                    continue
                } else {
                    var t135 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t136 int32 = t135 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t136)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop132
        }
    }
    var t98 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t98)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t99 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t99)
    var t100 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t100)
    var for_iter41 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(pairs__9)
    Loop_loop128:
    for {
        if true {
            var for_next42 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(for_iter41)
            switch for_next42.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop128
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x43 Tuple2_5int32_6string = for_next42.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x44 int32 = x43._0
                var x45 string = x43._1
                var text__11 string = x45
                var number__10 int32 = x44
                var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t131 string = t130 + text__11
                println__T_string(t131)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop128
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t102 FnIterator__int32 = counted_range(calls__12)
    var for_iter48 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t102)
    Loop_loop124:
    for {
        if true {
            var for_next49 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter48)
            switch for_next49.(type) {
            case Option__int32_None:
                break Loop_loop124
            case Option__int32_Some:
                var x50 int32 = for_next49.(Option__int32_Some)._0
                var value__14 int32 = x50
                var t126 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
                var t127 int32 = t126 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(range_sum__13, t127)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop124
        }
    }
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t104)
    var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
    println__T_int32(t105)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t106 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter55 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(t106)
    Loop_loop120:
    for {
        if true {
            var for_next56 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter55)
            switch for_next56.(type) {
            case Option__int32_None:
                break Loop_loop120
            case Option__int32_Some:
                var x57 int32 = for_next56.(Option__int32_Some)._0
                var value__16 int32 = x57
                var t122 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t123 int32 = t122 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t123)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop120
        }
    }
    var t108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t108)
    var t109 FnIterator__int32 = countdown(4)
    var for_iter61 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t109)
    Loop_loop116:
    for {
        if true {
            var for_next62 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter61)
            switch for_next62.(type) {
            case Option__int32_None:
                break Loop_loop116
            case Option__int32_Some:
                var x63 int32 = for_next62.(Option__int32_Some)._0
                var value__17 int32 = x63
                var t119 bool = value__17 == 2
                if t119 {
                    break Loop_loop116
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop116
        }
    }
    var empty__18 FnIterator__int32 = _goml_m_range(0, 0)
    var for_iter67 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(empty__18)
    Loop_loop114:
    for {
        if true {
            var for_next68 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter67)
            switch for_next68.(type) {
            case Option__int32_None:
                break Loop_loop114
            case Option__int32_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop114
        }
    }
    var t112 FnIterator__int32 = _goml_m_range(3, 8)
    var t113 int32 = first_even(t112)
    println__T_int32(t113)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv139 *ref_int32_x
    var t140 *ref_int32_x = ref__Ref_5int32(value__137)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv142 int32
    var t143 int32 = ref_get__Ref_5int32(self__138)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__68 func() Option__int32) FnIterator__int32 {
    var retv147 FnIterator__int32
    var t148 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__68,
    }
    retv147 = t148
    return retv147
}

func _goml_m_range(start__141 int32, end__142 int32) FnIterator__int32 {
    var retv150 FnIterator__int32
    var current__143 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__141)
    var t151 closure_env_range_1 = closure_env_range_1{
        current_0: current__143,
        end_1: end__142,
    }
    var t152 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(t151)
    })
    retv150 = t152
    return retv150
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__76 FnIterator__int32) FnIterator__int32 {
    var retv154 FnIterator__int32
    retv154 = self__76
    return retv154
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__69 FnIterator__int32) Option__int32 {
    var retv156 Option__int32
    var t157 func() Option__int32 = self__69.next_fn
    var t158 Option__int32 = t157()
    retv156 = t158
    return retv156
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv160 *_goml_vec_int32
    var t161 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__94 *_goml_vec_int32, elem__95 int32) struct{} {
    vec_push__Vec_5int32(self__94, elem__95)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__114 *_goml_vec_int32) FnIterator__int32 {
    var retv165 FnIterator__int32
    var t166 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__114)
    retv165 = t166
    return retv165
}

func println__T_int32(value__1 int32) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv171 *_goml_vec_Tuple2_5int32_6string
    var t172 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__94 *_goml_vec_Tuple2_5int32_6string, elem__95 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__94, elem__95)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(self__114 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv176 _goml_m_FnIterator_____o_int32_c_string_q_
    var t177 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__114)
    retv176 = t177
    return retv176
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(self__69 _goml_m_FnIterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv179 _goml_m_Option_____o_int32_c_string_q_
    var t180 func() _goml_m_Option_____o_int32_c_string_q_ = self__69.next_fn
    var t181 _goml_m_Option_____o_int32_c_string_q_ = t180()
    retv179 = t181
    return retv179
}

func println__T_string(value__1 string) struct{} {
    var t183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv186 string
    var t187 string = _goml_runtime_core_int32_to_string(self__2)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__106 *_goml_vec_int32, start__107 int32, end__108 int32) []int32 {
    var retv189 []int32
    var t190 []int32 = self__106.items[start__107:end__108]
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__126 []int32) FnIterator__int32 {
    var retv192 FnIterator__int32
    var t193 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__126)
    retv192 = t193
    return retv192
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__109 *_goml_vec_int32) FnIterator__int32 {
    var retv195 FnIterator__int32
    var index__110 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__111 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__109)
    var t196 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__110,
        len_1: len__111,
        self_2: self__109,
    }
    var t197 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t196)
    })
    retv195 = t197
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv199 string
    var t200 string = _goml_runtime_core_int32_to_string(self__13)
    retv199 = t200
    return retv199
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__109 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv202 _goml_m_FnIterator_____o_int32_c_string_q_
    var index__110 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__111 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__109)
    var t203 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__110,
        len_1: len__111,
        self_2: self__109,
    }
    var t204 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t203)
    })
    retv202 = t204
    return retv202
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv206 string
    retv206 = self__9
    return retv206
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__121 []int32) FnIterator__int32 {
    var retv208 FnIterator__int32
    var index__122 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__123 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__121)
    var t209 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__122,
        len_1: len__123,
        self_2: self__121,
    }
    var t210 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t209)
    })
    retv208 = t210
    return retv208
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__105 *_goml_vec_int32) int32 {
    var retv212 int32
    var t213 int32 = vec_len__Vec_5int32(self__105)
    retv212 = t213
    return retv212
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__100 *_goml_vec_int32, index__101 int32) int32 {
    var retv215 int32
    var t216 int32 = vec_get__Vec_5int32(self__100, index__101)
    retv215 = t216
    return retv215
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__105 *_goml_vec_Tuple2_5int32_6string) int32 {
    var retv218 int32
    var t219 int32 = vec_len__Vec_21Tuple2_5int32_6string(self__105)
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__100 *_goml_vec_Tuple2_5int32_6string, index__101 int32) Tuple2_5int32_6string {
    var retv221 Tuple2_5int32_6string
    var t222 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__100, index__101)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(next_fn__68 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv224 _goml_m_FnIterator_____o_int32_c_string_q_
    var t225 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_FnIterator_____o_int32_c_string_q_{
        next_fn: next_fn__68,
    }
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__117 []int32) int32 {
    var retv227 int32
    var t228 int32 = int32(len(self__117))
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__115 []int32, index__116 int32) int32 {
    var retv230 int32
    var t231 int32 = self__115[index__116]
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env73 closure_env_countdown_0) Option__int32 {
    var retv239 Option__int32
    var current__1 *ref_int32_x = env73.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t242 bool = value__2 > 0
    var jp241 Option__int32
    if t242 {
        var t243 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t243)
        var t244 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp241 = t244
    } else {
        jp241 = Option__int32_None{}
    }
    retv239 = jp241
    return retv239
}

func _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(env74 closure_env_range_1) Option__int32 {
    var retv246 Option__int32
    var current__143 *ref_int32_x = env74.current_0
    var end__142 int32 = env74.end_1
    var value__144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__143)
    var t249 bool = value__144 < end__142
    var jp248 Option__int32
    if t249 {
        var t250 int32 = value__144 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__143, t250)
        var t251 Option__int32 = Option__int32_Some{
            _0: value__144,
        }
        jp248 = t251
    } else {
        jp248 = Option__int32_None{}
    }
    retv246 = jp248
    return retv246
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env75 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv253 Option__int32
    var index__110 *ref_int32_x = env75.index_0
    var len__111 int32 = env75.len_1
    var self__109 *_goml_vec_int32 = env75.self_2
    var current__112 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__110)
    var t256 bool = current__112 < len__111
    var jp255 Option__int32
    if t256 {
        var value__113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__109, current__112)
        var t257 int32 = current__112 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__110, t257)
        var t258 Option__int32 = Option__int32_Some{
            _0: value__113,
        }
        jp255 = t258
    } else {
        jp255 = Option__int32_None{}
    }
    retv253 = jp255
    return retv253
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env76 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv260 _goml_m_Option_____o_int32_c_string_q_
    var index__110 *ref_int32_x = env76.index_0
    var len__111 int32 = env76.len_1
    var self__109 *_goml_vec_Tuple2_5int32_6string = env76.self_2
    var current__112 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__110)
    var t263 bool = current__112 < len__111
    var jp262 _goml_m_Option_____o_int32_c_string_q_
    if t263 {
        var value__113 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__109, current__112)
        var t264 int32 = current__112 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__110, t264)
        var t265 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__113,
        }
        jp262 = t265
    } else {
        jp262 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv260 = jp262
    return retv260
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env77 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv267 Option__int32
    var index__122 *ref_int32_x = env77.index_0
    var len__123 int32 = env77.len_1
    var self__121 []int32 = env77.self_2
    var current__124 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__122)
    var t270 bool = current__124 < len__123
    var jp269 Option__int32
    if t270 {
        var value__125 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__121, current__124)
        var t271 int32 = current__124 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__122, t271)
        var t272 Option__int32 = Option__int32_Some{
            _0: value__125,
        }
        jp269 = t272
    } else {
        jp269 = Option__int32_None{}
    }
    retv267 = jp269
    return retv267
}

func main() {
    main0()
}
