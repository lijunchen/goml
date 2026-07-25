package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func vec_get__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, index int) Tuple2_5int32_6string {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string) int {
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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type _goml_m_FnIterator_____o_int32_c_string_q_ struct {
    next_fn func() _goml_m_Option_____o_int32_c_string_q_
}

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_2 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_Tuple2_5int32_6string
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_4 struct {
    index_0 *ref_int_x
    len_1 int
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

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

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
    var retv121 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t122 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t123 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t122)
    })
    retv121 = t123
    return retv121
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var retv125 FnIterator__int
    var t126 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t127 int32 = t126 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t127)
    var t128 FnIterator__int = _goml_m_range(1, 5)
    retv125 = t128
    return retv125
}

func first_even(values__4 FnIterator__int) int {
    var retv130 int
    var for_iter66 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop132:
    for {
        if true {
            var for_next67 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter66)
            switch for_next67.(type) {
            case Option__int_None:
                break Loop_loop132
            case Option__int_Some:
                var x68 int = for_next67.(Option__int_Some)._0
                var value__5 int = x68
                var t135 int = value__5 / 2
                var t136 int = t135 * 2
                var t137 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t136, value__5)
                if t137 {
                    retv130 = value__5
                    return retv130
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop132
        }
    }
    retv130 = -1
    return retv130
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter74 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(values__6)
    Loop_loop174:
    for {
        if true {
            var for_next75 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter74)
            switch for_next75.(type) {
            case Option__int32_None:
                break Loop_loop174
            case Option__int32_Some:
                var x76 int32 = for_next75.(Option__int32_Some)._0
                var value__8 int32 = x76
                var t179 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
                if t179 {
                    continue
                } else {
                    var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                    var t178 int32 = t177 + value__8
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t178)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop174
        }
    }
    var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t140)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t141 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t141)
    var t142 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t142)
    var for_iter83 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(pairs__9)
    Loop_loop170:
    for {
        if true {
            var for_next84 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(for_iter83)
            switch for_next84.(type) {
            case _goml_m_Option_____o_int32_c_string_q__None:
                break Loop_loop170
            case _goml_m_Option_____o_int32_c_string_q__Some:
                var x85 Tuple2_5int32_6string = for_next84.(_goml_m_Option_____o_int32_c_string_q__Some)._0
                var x86 int32 = x85._0
                var x87 string = x85._1
                var text__11 string = x87
                var number__10 int32 = x86
                var t172 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
                var t173 string = t172 + text__11
                println__T_string(t173)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop170
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t144 FnIterator__int = counted_range(calls__12)
    var for_iter90 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t144)
    Loop_loop166:
    for {
        if true {
            var for_next91 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter90)
            switch for_next91.(type) {
            case Option__int_None:
                break Loop_loop166
            case Option__int_Some:
                var x92 int = for_next91.(Option__int_Some)._0
                var value__14 int = x92
                var t168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
                var t169 int = t168 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t169)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop166
        }
    }
    var t146 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t146)
    var t147 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t147)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t148 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_iter97 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(t148)
    Loop_loop162:
    for {
        if true {
            var for_next98 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter97)
            switch for_next98.(type) {
            case Option__int32_None:
                break Loop_loop162
            case Option__int32_Some:
                var x99 int32 = for_next98.(Option__int32_Some)._0
                var value__16 int32 = x99
                var t164 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
                var t165 int32 = t164 + value__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t165)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop162
        }
    }
    var t150 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t150)
    var t151 FnIterator__int32 = countdown(4)
    var for_iter103 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t151)
    Loop_loop158:
    for {
        if true {
            var for_next104 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter103)
            switch for_next104.(type) {
            case Option__int32_None:
                break Loop_loop158
            case Option__int32_Some:
                var x105 int32 = for_next104.(Option__int32_Some)._0
                var value__17 int32 = x105
                var t161 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t161 {
                    break Loop_loop158
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop158
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter109 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop156:
    for {
        if true {
            var for_next110 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter109)
            switch for_next110.(type) {
            case Option__int_None:
                break Loop_loop156
            case Option__int_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop156
        }
    }
    var t154 FnIterator__int = _goml_m_range(3, 8)
    var t155 int = first_even(t154)
    println__T_int(t155)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv181 *ref_int32_x
    var t182 *ref_int32_x = ref__Ref_5int32(value__209)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv184 int32
    var t185 int32 = ref_get__Ref_5int32(self__210)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv189 FnIterator__int32
    var t190 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv189 = t190
    return retv189
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv192 FnIterator__int
    var t193 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv195 FnIterator__int
    retv195 = self__109
    return retv195
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv197 Option__int
    var t198 func() Option__int = self__102.next_fn
    var t199 Option__int = t198()
    retv197 = t199
    return retv197
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv201 bool
    var t202 bool = self__59 == other__60
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv204 *_goml_vec_int32
    var t205 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv204 = t205
    return retv204
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__185 *_goml_vec_int32) FnIterator__int32 {
    var retv209 FnIterator__int32
    var t210 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__185)
    retv209 = t210
    return retv209
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv212 Option__int32
    var t213 func() Option__int32 = self__102.next_fn
    var t214 Option__int32 = t213()
    retv212 = t214
    return retv212
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv216 bool
    var t217 bool = self__65 == other__66
    retv216 = t217
    return retv216
}

func println__T_int32(value__1 int32) struct{} {
    var t219 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv222 *_goml_vec_Tuple2_5int32_6string
    var t223 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__128 *_goml_vec_Tuple2_5int32_6string, elem__129 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_int32_c_string_q__r__i_into__iter(self__185 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv227 _goml_m_FnIterator_____o_int32_c_string_q_
    var t228 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__185)
    retv227 = t228
    return retv227
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_string_q__i_next(self__102 _goml_m_FnIterator_____o_int32_c_string_q_) _goml_m_Option_____o_int32_c_string_q_ {
    var retv230 _goml_m_Option_____o_int32_c_string_q_
    var t231 func() _goml_m_Option_____o_int32_c_string_q_ = self__102.next_fn
    var t232 _goml_m_Option_____o_int32_c_string_q_ = t231()
    retv230 = t232
    return retv230
}

func println__T_string(value__1 string) struct{} {
    var t234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv237 string
    var t238 string = _goml_runtime_core_int32_to_string(self__6)
    retv237 = t238
    return retv237
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv240 *ref_int_x
    var t241 *ref_int_x = ref__Ref_3int(value__209)
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv243 int
    var t244 int = ref_get__Ref_3int(self__210)
    retv243 = t244
    return retv243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t248 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t248)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__177 *_goml_vec_int32, start__178 int, end__179 int) []int32 {
    var retv251 []int32
    var t252 []int32 = self__177.items[start__178:end__179]
    retv251 = t252
    return retv251
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__197 []int32) FnIterator__int32 {
    var retv254 FnIterator__int32
    var t255 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__197)
    retv254 = t255
    return retv254
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    var retv257 FnIterator__int32
    retv257 = self__109
    return retv257
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv259 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t260 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__222,
        end_1: end__221,
    }
    var t261 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t260)
    })
    retv259 = t261
    return retv259
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__180 *_goml_vec_int32) FnIterator__int32 {
    var retv263 FnIterator__int32
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__180)
    var t264 closure_env_inherent_Vec_Vec_T_iter_T_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_int32_2{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t265 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(t264)
    })
    retv263 = t265
    return retv263
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv267 string
    var t268 string = _goml_runtime_core_int32_to_string(self__43)
    retv267 = t268
    return retv267
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_int32_c_string_q_(self__180 *_goml_vec_Tuple2_5int32_6string) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv270 _goml_m_FnIterator_____o_int32_c_string_q_
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__180)
    var t271 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3 = closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t272 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(func() _goml_m_Option_____o_int32_c_string_q_ {
        return _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(t271)
    })
    retv270 = t272
    return retv270
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv274 string
    retv274 = self__38
    return retv274
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv276 string
    var t277 string = _goml_runtime_core_int_to_string(self__40)
    retv276 = t277
    return retv276
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__192 []int32) FnIterator__int32 {
    var retv279 FnIterator__int32
    var index__193 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__194 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__192)
    var t280 closure_env_inherent_Slice_Slice_T_iter_T_int32_4 = closure_env_inherent_Slice_Slice_T_iter_T_int32_4{
        index_0: index__193,
        len_1: len__194,
        self_2: self__192,
    }
    var t281 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(t280)
    })
    retv279 = t281
    return retv279
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv283 FnIterator__int
    var t284 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv283 = t284
    return retv283
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv286 int
    var t287 int = vec_len__Vec_5int32(self__139)
    retv286 = t287
    return retv286
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv289 int32
    var t290 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv289 = t290
    return retv289
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int32_c_string_q_(self__139 *_goml_vec_Tuple2_5int32_6string) int {
    var retv292 int
    var t293 int = vec_len__Vec_21Tuple2_5int32_6string(self__139)
    retv292 = t293
    return retv292
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__134 *_goml_vec_Tuple2_5int32_6string, index__135 int) Tuple2_5int32_6string {
    var retv295 Tuple2_5int32_6string
    var t296 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(self__134, index__135)
    retv295 = t296
    return retv295
}

func _goml_m_inherent_i_FnIterator__h63b284beb36abfa28c563ce1e4609856_t32_c_string_q_(next_fn__101 func() _goml_m_Option_____o_int32_c_string_q_) _goml_m_FnIterator_____o_int32_c_string_q_ {
    var retv298 _goml_m_FnIterator_____o_int32_c_string_q_
    var t299 _goml_m_FnIterator_____o_int32_c_string_q_ = _goml_m_FnIterator_____o_int32_c_string_q_{
        next_fn: next_fn__101,
    }
    retv298 = t299
    return retv298
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__188 []int32) int {
    var retv301 int
    var t302 int = len(self__188)
    retv301 = t302
    return retv301
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__186 []int32, index__187 int) int32 {
    var retv304 int32
    var t305 int32 = self__186[index__187]
    retv304 = t305
    return retv304
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env115 closure_env_countdown_0) Option__int32 {
    var retv313 Option__int32
    var current__1 *ref_int32_x = env115.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t316 bool = value__2 > 0
    var jp315 Option__int32
    if t316 {
        var t317 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t317)
        var t318 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp315 = t318
    } else {
        jp315 = Option__int32_None{}
    }
    retv313 = jp315
    return retv313
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env116 closure_env_goml_builtin_range_1) Option__int {
    var retv320 Option__int
    var current__222 *ref_int_x = env116.current_0
    var end__221 int = env116.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t323 bool = value__223 < end__221
    var jp322 Option__int
    if t323 {
        var t324 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t324)
        var t325 Option__int = Option__int_Some{
            _0: value__223,
        }
        jp322 = t325
    } else {
        jp322 = Option__int_None{}
    }
    retv320 = jp322
    return retv320
}

func _goml_m_inherent_i_closure__en_h00bef297752a27142cba073027f15d16_nt32__2_i_apply(env117 closure_env_inherent_Vec_Vec_T_iter_T_int32_2) Option__int32 {
    var retv327 Option__int32
    var index__181 *ref_int_x = env117.index_0
    var len__182 int = env117.len_1
    var self__180 *_goml_vec_int32 = env117.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t330 bool = current__183 < len__182
    var jp329 Option__int32
    if t330 {
        var value__184 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__180, current__183)
        var t331 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t331)
        var t332 Option__int32 = Option__int32_Some{
            _0: value__184,
        }
        jp329 = t332
    } else {
        jp329 = Option__int32_None{}
    }
    retv327 = jp329
    return retv327
}

func _goml_m_inherent_i_closure__en_h9e94a40794857ae90c2f66a8320a6b99_ring__3_i_apply(env118 closure_env_inherent_Vec_Vec_T_iter_T_int32_string_3) _goml_m_Option_____o_int32_c_string_q_ {
    var retv334 _goml_m_Option_____o_int32_c_string_q_
    var index__181 *ref_int_x = env118.index_0
    var len__182 int = env118.len_1
    var self__180 *_goml_vec_Tuple2_5int32_6string = env118.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t337 bool = current__183 < len__182
    var jp336 _goml_m_Option_____o_int32_c_string_q_
    if t337 {
        var value__184 Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_int32_c_string_q_(self__180, current__183)
        var t338 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t338)
        var t339 _goml_m_Option_____o_int32_c_string_q_ = _goml_m_Option_____o_int32_c_string_q__Some{
            _0: value__184,
        }
        jp336 = t339
    } else {
        jp336 = _goml_m_Option_____o_int32_c_string_q__None{}
    }
    retv334 = jp336
    return retv334
}

func _goml_m_inherent_i_closure__en_hc9e1f0b61eb13cebdf11cab19cef8d84_nt32__4_i_apply(env119 closure_env_inherent_Slice_Slice_T_iter_T_int32_4) Option__int32 {
    var retv341 Option__int32
    var index__193 *ref_int_x = env119.index_0
    var len__194 int = env119.len_1
    var self__192 []int32 = env119.self_2
    var current__195 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__193)
    var t344 bool = current__195 < len__194
    var jp343 Option__int32
    if t344 {
        var value__196 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__192, current__195)
        var t345 int = current__195 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__193, t345)
        var t346 Option__int32 = Option__int32_Some{
            _0: value__196,
        }
        jp343 = t346
    } else {
        jp343 = Option__int32_None{}
    }
    retv341 = jp343
    return retv341
}

func main() {
    main0()
}
