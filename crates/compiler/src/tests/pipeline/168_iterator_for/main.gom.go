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
    var for_iter24 FnIterator__int32 = values__4
    Loop_loop90:
    for {
        if true {
            var for_next25 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter24)
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
    var for_iter32 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(values__6)
    Loop_loop130:
    for {
        if true {
            var for_next33 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter32)
            switch for_next33.(type) {
            case Option__int32_None:
                break Loop_loop130
            case Option__int32_Some:
                var x34 int32 = for_next33.(Option__int32_Some)._0
                var value__8 int32 = x34
                var t135 bool = value__8 == 20
                if t135 {
                    continue
                } else {
                    var t133 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t134 int32 = t133 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t134)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop130
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
    var for_iter41 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(pairs__9)
    Loop_loop126:
    for {
        if true {
            var for_next42 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_trait__impl_i_Iterator_h9270cbd89c5a69a5246c6b4563eb6bdf_tring_q__i_next(for_iter41)
            switch for_next42.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop126
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x43 Tuple2_5int32_6string = for_next42.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x44 int32 = x43._0
                var x45 string = x43._1
                var text__11 string = x45
                var number__10 int32 = x44
                var t128 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t129 string = t128 + text__11
                println__T_string(t129)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop126
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter48 FnIterator__int32 = counted_range(calls__12)
    Loop_loop122:
    for {
        if true {
            var for_next49 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter48)
            switch for_next49.(type) {
            case Option__int32_None:
                break Loop_loop122
            case Option__int32_Some:
                var x50 int32 = for_next49.(Option__int32_Some)._0
                var value__14 int32 = x50
                var t124 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
                var t125 int32 = t124 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(range_sum__13, t125)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop122
        }
    }
    var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t103)
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
    println__T_int32(t104)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t105 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter55 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(t105)
    Loop_loop118:
    for {
        if true {
            var for_next56 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter55)
            switch for_next56.(type) {
            case Option__int32_None:
                break Loop_loop118
            case Option__int32_Some:
                var x57 int32 = for_next56.(Option__int32_Some)._0
                var value__16 int32 = x57
                var t120 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t121 int32 = t120 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t121)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop118
        }
    }
    var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t107)
    var for_iter61 FnIterator__int32 = countdown(4)
    Loop_loop114:
    for {
        if true {
            var for_next62 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter61)
            switch for_next62.(type) {
            case Option__int32_None:
                break Loop_loop114
            case Option__int32_Some:
                var x63 int32 = for_next62.(Option__int32_Some)._0
                var value__17 int32 = x63
                var t117 bool = value__17 == 2
                if t117 {
                    break Loop_loop114
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop114
        }
    }
    var empty__18 FnIterator__int32 = _goml_m_range(0, 0)
    var for_iter67 FnIterator__int32 = empty__18
    Loop_loop112:
    for {
        if true {
            var for_next68 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(for_iter67)
            switch for_next68.(type) {
            case Option__int32_None:
                break Loop_loop112
            case Option__int32_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop112
        }
    }
    var t110 FnIterator__int32 = _goml_m_range(3, 8)
    var t111 int32 = first_even(t110)
    println__T_int32(t111)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv137 *ref_int32_x
    var t138 *ref_int32_x = ref__Ref_5int32(value__137)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv140 int32
    var t141 int32 = ref_get__Ref_5int32(self__138)
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__71 func() Option__int32) FnIterator__int32 {
    var retv145 FnIterator__int32
    var t146 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__71,
    }
    retv145 = t146
    return retv145
}

func _goml_m_range(start__141 int32, end__142 int32) FnIterator__int32 {
    var retv148 FnIterator__int32
    var current__143 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__141)
    var t149 closure_env_range_1 = closure_env_range_1{
        current_0: current__143,
        end_1: end__142,
    }
    var t150 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(t149)
    })
    retv148 = t150
    return retv148
}

func _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(self__72 FnIterator__int32) Option__int32 {
    var retv152 Option__int32
    var t153 func() Option__int32 = self__72.next_fn
    var t154 Option__int32 = t153()
    retv152 = t154
    return retv152
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv156 *_goml_vec_int32
    var t157 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv156 = t157
    return retv156
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__96 *_goml_vec_int32, elem__97 int32) struct{} {
    vec_push__Vec_5int32(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__111 *_goml_vec_int32) FnIterator__int32 {
    var retv161 FnIterator__int32
    var index__112 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__111)
    var t162 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__112,
        len_1: len__113,
        self_2: self__111,
    }
    var t163 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t162)
    })
    retv161 = t163
    return retv161
}

func println__T_int32(value__1 int32) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv168 *_goml_vec_Tuple2_5int32_6string
    var t169 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__96 *_goml_vec_Tuple2_5int32_6string, elem__97 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__111 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv173 _goml_m_FnIterator_____o_int32_c_string_q_
    var index__112 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__111)
    var t174 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__112,
        len_1: len__113,
        self_2: self__111,
    }
    var t175 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t174)
    })
    retv173 = t175
    return retv173
}

func _goml_m_trait__impl_i_Iterator_h9270cbd89c5a69a5246c6b4563eb6bdf_tring_q__i_next(self__72 _goml_m_FnIterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv177 _goml_m_Option_____o_int32_c_string_q_
    var t178 func() _goml_m_Option_____o_int32_c_string_q_ = self__72.next_fn
    var t179 _goml_m_Option_____o_int32_c_string_q_ = t178()
    retv177 = t179
    return retv177
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv184 string
    var t185 string = _goml_runtime_core_int32_to_string(self__2)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__108 *_goml_vec_int32, start__109 int32, end__110 int32) []int32 {
    var retv187 []int32
    var t188 []int32 = self__108.items[start__109:end__110]
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__122 []int32) FnIterator__int32 {
    var retv190 FnIterator__int32
    var index__123 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__124 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__122)
    var t191 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__123,
        len_1: len__124,
        self_2: self__122,
    }
    var t192 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t191)
    })
    retv190 = t192
    return retv190
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__107 *_goml_vec_int32) int32 {
    var retv194 int32
    var t195 int32 = vec_len__Vec_5int32(self__107)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__102 *_goml_vec_int32, index__103 int32) int32 {
    var retv197 int32
    var t198 int32 = vec_get__Vec_5int32(self__102, index__103)
    retv197 = t198
    return retv197
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv200 string
    var t201 string = _goml_runtime_core_int32_to_string(self__13)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__107 *_goml_vec_Tuple2_5int32_6string) int32 {
    var retv203 int32
    var t204 int32 = vec_len__Vec_21Tuple2_5int32_6string(self__107)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__102 *_goml_vec_Tuple2_5int32_6string, index__103 int32) Tuple2_5int32_6string {
    var retv206 Tuple2_5int32_6string
    var t207 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__102, index__103)
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(next_fn__71 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv209 _goml_m_FnIterator_____o_int32_c_string_q_
    var t210 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_FnIterator_____o_int32_c_string_q_{
        next_fn: next_fn__71,
    }
    retv209 = t210
    return retv209
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv212 string
    retv212 = self__9
    return retv212
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__118 []int32) int32 {
    var retv214 int32
    var t215 int32 = int32(len(self__118))
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__116 []int32, index__117 int32) int32 {
    var retv217 int32
    var t218 int32 = self__116[index__117]
    retv217 = t218
    return retv217
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env73 closure_env_countdown_0) Option__int32 {
    var retv226 Option__int32
    var current__1 *ref_int32_x = env73.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t229 bool = value__2 > 0
    var jp228 Option__int32
    if t229 {
        var t230 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t230)
        var t231 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp228 = t231
    } else {
        jp228 = Option__int32_None{}
    }
    retv226 = jp228
    return retv226
}

func _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(env74 closure_env_range_1) Option__int32 {
    var retv233 Option__int32
    var current__143 *ref_int32_x = env74.current_0
    var end__142 int32 = env74.end_1
    var value__144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__143)
    var t236 bool = value__144 < end__142
    var jp235 Option__int32
    if t236 {
        var t237 int32 = value__144 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__143, t237)
        var t238 Option__int32 = Option__int32_Some{
            _0: value__144,
        }
        jp235 = t238
    } else {
        jp235 = Option__int32_None{}
    }
    retv233 = jp235
    return retv233
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env75 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv240 Option__int32
    var index__112 *ref_int32_x = env75.index_0
    var len__113 int32 = env75.len_1
    var self__111 *_goml_vec_int32 = env75.self_2
    var current__114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__112)
    var t243 bool = current__114 < len__113
    var jp242 Option__int32
    if t243 {
        var value__115 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__111, current__114)
        var t244 int32 = current__114 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__112, t244)
        var t245 Option__int32 = Option__int32_Some{
            _0: value__115,
        }
        jp242 = t245
    } else {
        jp242 = Option__int32_None{}
    }
    retv240 = jp242
    return retv240
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env76 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv247 _goml_m_Option_____o_int32_c_string_q_
    var index__112 *ref_int32_x = env76.index_0
    var len__113 int32 = env76.len_1
    var self__111 *_goml_vec_Tuple2_5int32_6string = env76.self_2
    var current__114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__112)
    var t250 bool = current__114 < len__113
    var jp249 _goml_m_Option_____o_int32_c_string_q_
    if t250 {
        var value__115 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__111, current__114)
        var t251 int32 = current__114 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__112, t251)
        var t252 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__115,
        }
        jp249 = t252
    } else {
        jp249 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv247 = jp249
    return retv247
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env77 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv254 Option__int32
    var index__123 *ref_int32_x = env77.index_0
    var len__124 int32 = env77.len_1
    var self__122 []int32 = env77.self_2
    var current__125 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__123)
    var t257 bool = current__125 < len__124
    var jp256 Option__int32
    if t257 {
        var value__126 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__122, current__125)
        var t258 int32 = current__125 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__123, t258)
        var t259 Option__int32 = Option__int32_Some{
            _0: value__126,
        }
        jp256 = t259
    } else {
        jp256 = Option__int32_None{}
    }
    retv254 = jp256
    return retv254
}

func main() {
    main0()
}
