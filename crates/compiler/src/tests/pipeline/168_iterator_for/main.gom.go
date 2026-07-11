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

type Iterator__int32 struct {
    next_fn func() Option__int32
}

type _goml_m_Iterator_____o_int32_c_string_q_ struct {
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

func countdown(start__0 int32) Iterator__int32 {
    var retv64 Iterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t65 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t66 Iterator__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t65)
    })
    retv64 = t66
    return retv64
}

func counted_range(calls__3 *ref_int32_x) Iterator__int32 {
    var retv68 Iterator__int32
    var t69 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t70 int32 = t69 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t70)
    var t71 Iterator__int32 = _goml_m_range(1, 5)
    retv68 = t71
    return retv68
}

func first_even(values__4 Iterator__int32) int32 {
    var retv73 int32
    var for_iter9 Iterator__int32 = values__4
    Loop_loop75:
    for {
        if true {
            var for_next10 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter9)
            switch for_next10.(type) {
            case Option__int32_None:
                break Loop_loop75
            case Option__int32_Some:
                var x11 int32 = for_next10.(Option__int32_Some)._0
                var value__5 int32 = x11
                var t78 int32 = value__5 / 2
                var t79 int32 = t78 * 2
                var t80 bool = t79 == value__5
                if t80 {
                    retv73 = value__5
                    return retv73
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop75
        }
    }
    retv73 = -1
    return retv73
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter17 Iterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(values__6)
    Loop_loop115:
    for {
        if true {
            var for_next18 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter17)
            switch for_next18.(type) {
            case Option__int32_None:
                break Loop_loop115
            case Option__int32_Some:
                var x19 int32 = for_next18.(Option__int32_Some)._0
                var value__8 int32 = x19
                var t120 bool = value__8 == 20
                if t120 {
                    continue
                } else {
                    var t118 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t119 int32 = t118 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t119)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop115
        }
    }
    var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t83)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t84 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t84)
    var t85 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t85)
    var for_iter26 _goml_m_Iterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(pairs__9)
    Loop_loop111:
    for {
        if true {
            var for_next27 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T___o_int32_c_string_q_(for_iter26)
            switch for_next27.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop111
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x28 Tuple2_5int32_6string = for_next27.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x29 int32 = x28._0
                var x30 string = x28._1
                var text__11 string = x30
                var number__10 int32 = x29
                var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t114 string = t113 + text__11
                println__T_string(t114)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop111
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter33 Iterator__int32 = counted_range(calls__12)
    Loop_loop107:
    for {
        if true {
            var for_next34 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter33)
            switch for_next34.(type) {
            case Option__int32_None:
                break Loop_loop107
            case Option__int32_Some:
                var x35 int32 = for_next34.(Option__int32_Some)._0
                var value__14 int32 = x35
                var t109 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
                var t110 int32 = t109 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(range_sum__13, t110)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop107
        }
    }
    var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t88)
    var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(range_sum__13)
    println__T_int32(t89)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t90 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter40 Iterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(t90)
    Loop_loop103:
    for {
        if true {
            var for_next41 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter40)
            switch for_next41.(type) {
            case Option__int32_None:
                break Loop_loop103
            case Option__int32_Some:
                var x42 int32 = for_next41.(Option__int32_Some)._0
                var value__16 int32 = x42
                var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t106 int32 = t105 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t106)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop103
        }
    }
    var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t92)
    var for_iter46 Iterator__int32 = countdown(4)
    Loop_loop99:
    for {
        if true {
            var for_next47 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter46)
            switch for_next47.(type) {
            case Option__int32_None:
                break Loop_loop99
            case Option__int32_Some:
                var x48 int32 = for_next47.(Option__int32_Some)._0
                var value__17 int32 = x48
                var t102 bool = value__17 == 2
                if t102 {
                    break Loop_loop99
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop99
        }
    }
    var empty__18 Iterator__int32 = _goml_m_range(0, 0)
    var for_iter52 Iterator__int32 = empty__18
    Loop_loop97:
    for {
        if true {
            var for_next53 Option__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(for_iter52)
            switch for_next53.(type) {
            case Option__int32_None:
                break Loop_loop97
            case Option__int32_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop97
        }
    }
    var t95 Iterator__int32 = _goml_m_range(3, 8)
    var t96 int32 = first_even(t95)
    println__T_int32(t96)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv122 *ref_int32_x
    var t123 *ref_int32_x = ref__Ref_5int32(value__114)
    retv122 = t123
    return retv122
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv125 int32
    var t126 int32 = ref_get__Ref_5int32(self__115)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_from__fn____T__int32(next_fn__71 func() Option__int32) Iterator__int32 {
    var retv130 Iterator__int32
    var t131 Iterator__int32 = Iterator__int32{
        next_fn: next_fn__71,
    }
    retv130 = t131
    return retv130
}

func _goml_m_range(start__118 int32, end__119 int32) Iterator__int32 {
    var retv133 Iterator__int32
    var current__120 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__118)
    var t134 closure_env_range_1 = closure_env_range_1{
        current_0: current__120,
        end_1: end__119,
    }
    var t135 Iterator__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(t134)
    })
    retv133 = t135
    return retv133
}

func _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T__int32(self__72 Iterator__int32) Option__int32 {
    var retv137 Option__int32
    var t138 func() Option__int32 = self__72.next_fn
    var t139 Option__int32 = t138()
    retv137 = t139
    return retv137
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv141 *_goml_vec_int32
    var t142 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv141 = t142
    return retv141
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__73 *_goml_vec_int32, elem__74 int32) struct{} {
    vec_push__Vec_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__88 *_goml_vec_int32) Iterator__int32 {
    var retv146 Iterator__int32
    var index__89 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__90 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__88)
    var t147 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__89,
        len_1: len__90,
        self_2: self__88,
    }
    var t148 Iterator__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t147)
    })
    retv146 = t148
    return retv146
}

func println__T_int32(value__1 int32) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv153 *_goml_vec_Tuple2_5int32_6string
    var t154 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__73 *_goml_vec_Tuple2_5int32_6string, elem__74 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__88 *_goml_vec_Tuple2_5int32_6string) _goml_m_Iterator_____o_int32_c_string_q_ {
    var retv158 _goml_m_Iterator_____o_int32_c_string_q_
    var index__89 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__90 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__88)
    var t159 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__89,
        len_1: len__90,
        self_2: self__88,
    }
    var t160 _goml_m_Iterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Iterator_i__hd272d60c62a60eba4434710733e29bab_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t159)
    })
    retv158 = t160
    return retv158
}

func _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_next____T___o_int32_c_string_q_(self__72 _goml_m_Iterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv162 _goml_m_Option_____o_int32_c_string_q_
    var t163 func() _goml_m_Option_____o_int32_c_string_q_ = self__72.next_fn
    var t164 _goml_m_Option_____o_int32_c_string_q_ = t163()
    retv162 = t164
    return retv162
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int32_to_string(self__2)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__85 *_goml_vec_int32, start__86 int32, end__87 int32) []int32 {
    var retv172 []int32
    var t173 []int32 = self__85.items[start__86:end__87]
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__99 []int32) Iterator__int32 {
    var retv175 Iterator__int32
    var index__100 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__101 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__99)
    var t176 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__100,
        len_1: len__101,
        self_2: self__99,
    }
    var t177 Iterator__int32 = _goml_m_inherent_i_Iterator_i_Iterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t176)
    })
    retv175 = t177
    return retv175
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__84 *_goml_vec_int32) int32 {
    var retv179 int32
    var t180 int32 = vec_len__Vec_5int32(self__84)
    retv179 = t180
    return retv179
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__79 *_goml_vec_int32, index__80 int32) int32 {
    var retv182 int32
    var t183 int32 = vec_get__Vec_5int32(self__79, index__80)
    retv182 = t183
    return retv182
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv185 string
    var t186 string = _goml_runtime_core_int32_to_string(self__13)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__84 *_goml_vec_Tuple2_5int32_6string) int32 {
    var retv188 int32
    var t189 int32 = vec_len__Vec_21Tuple2_5int32_6string(self__84)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__79 *_goml_vec_Tuple2_5int32_6string, index__80 int32) Tuple2_5int32_6string {
    var retv191 Tuple2_5int32_6string
    var t192 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__79, index__80)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Iterator_i__hd272d60c62a60eba4434710733e29bab_t32_c_string_q_(next_fn__71 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_Iterator_____o_int32_c_string_q_ {
    var retv194 _goml_m_Iterator_____o_int32_c_string_q_
    var t195 _goml_m_Iterator_____o_int32_c_string_q_ = _goml_m_Iterator_____o_int32_c_string_q_{
        next_fn: next_fn__71,
    }
    retv194 = t195
    return retv194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv197 string
    retv197 = self__9
    return retv197
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__95 []int32) int32 {
    var retv199 int32
    var t200 int32 = int32(len(self__95))
    retv199 = t200
    return retv199
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__93 []int32, index__94 int32) int32 {
    var retv202 int32
    var t203 int32 = self__93[index__94]
    retv202 = t203
    return retv202
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env58 closure_env_countdown_0) Option__int32 {
    var retv211 Option__int32
    var current__1 *ref_int32_x = env58.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t214 bool = value__2 > 0
    var jp213 Option__int32
    if t214 {
        var t215 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t215)
        var t216 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp213 = t216
    } else {
        jp213 = Option__int32_None{}
    }
    retv211 = jp213
    return retv211
}

func _goml_m_inherent_i_closure__env__range__1_i_closure__env__range__1_i_apply(env59 closure_env_range_1) Option__int32 {
    var retv218 Option__int32
    var current__120 *ref_int32_x = env59.current_0
    var end__119 int32 = env59.end_1
    var value__121 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__120)
    var t221 bool = value__121 < end__119
    var jp220 Option__int32
    if t221 {
        var t222 int32 = value__121 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__120, t222)
        var t223 Option__int32 = Option__int32_Some{
            _0: value__121,
        }
        jp220 = t223
    } else {
        jp220 = Option__int32_None{}
    }
    retv218 = jp220
    return retv218
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env60 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv225 Option__int32
    var index__89 *ref_int32_x = env60.index_0
    var len__90 int32 = env60.len_1
    var self__88 *_goml_vec_int32 = env60.self_2
    var current__91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__89)
    var t228 bool = current__91 < len__90
    var jp227 Option__int32
    if t228 {
        var value__92 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__88, current__91)
        var t229 int32 = current__91 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__89, t229)
        var t230 Option__int32 = Option__int32_Some{
            _0: value__92,
        }
        jp227 = t230
    } else {
        jp227 = Option__int32_None{}
    }
    retv225 = jp227
    return retv225
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env61 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv232 _goml_m_Option_____o_int32_c_string_q_
    var index__89 *ref_int32_x = env61.index_0
    var len__90 int32 = env61.len_1
    var self__88 *_goml_vec_Tuple2_5int32_6string = env61.self_2
    var current__91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__89)
    var t235 bool = current__91 < len__90
    var jp234 _goml_m_Option_____o_int32_c_string_q_
    if t235 {
        var value__92 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__88, current__91)
        var t236 int32 = current__91 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__89, t236)
        var t237 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__92,
        }
        jp234 = t237
    } else {
        jp234 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv232 = jp234
    return retv232
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env62 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv239 Option__int32
    var index__100 *ref_int32_x = env62.index_0
    var len__101 int32 = env62.len_1
    var self__99 []int32 = env62.self_2
    var current__102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__100)
    var t242 bool = current__102 < len__101
    var jp241 Option__int32
    if t242 {
        var value__103 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__99, current__102)
        var t243 int32 = current__102 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__100, t243)
        var t244 Option__int32 = Option__int32_Some{
            _0: value__103,
        }
        jp241 = t244
    } else {
        jp241 = Option__int32_None{}
    }
    retv239 = jp241
    return retv239
}

func main() {
    main0()
}
