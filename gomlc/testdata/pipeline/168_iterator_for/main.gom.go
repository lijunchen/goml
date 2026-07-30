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

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
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

func countdown(start__0 int32) FnIterator__int32 {
    var retv128 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t129 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t130 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t129)
    })
    retv128 = t130
    return retv128
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var retv132 FnIterator__int
    var t133 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t134 int32 = t133 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t134)
    var t135 FnIterator__int = _goml_m_range(1, 5)
    retv132 = t135
    return retv132
}

func first_even(values__4 FnIterator__int) int {
    var retv137 int
    var for_iter70 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop139:
    for {
        if true {
            var for_next71 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter70)
            switch for_next71.(type) {
            case Option__int_None:
                break Loop_loop139
            case Option__int_Some:
                var x72 int = for_next71.(Option__int_Some)._0
                var value__5 int = x72
                var t142 int = value__5 / 2
                var t143 int = t142 * 2
                var t144 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t143, value__5)
                if t144 {
                    retv137 = value__5
                    return retv137
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop139
        }
    }
    retv137 = -1
    return retv137
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source78 *_goml_vec_int32 = values__6
    var for_limit79 int = vec_len__Vec_5int32(for_source78)
    var for_index80 int = 0
    Loop_loop182:
    for {
        var t183 bool = for_index80 < for_limit79
        if t183 {
            var for_item81 int32 = vec_get__Vec_5int32(for_source78, for_index80)
            var t184 int = for_index80 + 1
            for_index80 = t184
            var value__8 int32 = for_item81
            var t188 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
            if t188 {
                continue
            } else {
                var t186 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                var t187 int32 = t186 + value__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t187)
                continue
            }
        } else {
            break Loop_loop182
        }
    }
    var t147 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t147)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t148 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t148)
    var t149 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t149)
    var for_source89 *_goml_vec_Tuple2_5int32_6string = pairs__9
    var for_limit90 int = vec_len__Vec_21Tuple2_5int32_6string(for_source89)
    var for_index91 int = 0
    Loop_loop177:
    for {
        var t178 bool = for_index91 < for_limit90
        if t178 {
            var for_item92 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(for_source89, for_index91)
            var t179 int = for_index91 + 1
            for_index91 = t179
            var x94 int32 = for_item92._0
            var x95 string = for_item92._1
            var text__11 string = x95
            var number__10 int32 = x94
            var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
            var t181 string = t180 + text__11
            println__T_string(t181)
            continue
        } else {
            break Loop_loop177
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t151 FnIterator__int = counted_range(calls__12)
    var for_iter98 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t151)
    Loop_loop173:
    for {
        if true {
            var for_next99 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter98)
            switch for_next99.(type) {
            case Option__int_None:
                break Loop_loop173
            case Option__int_Some:
                var x100 int = for_next99.(Option__int_Some)._0
                var value__14 int = x100
                var t175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
                var t176 int = t175 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t176)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop173
        }
    }
    var t153 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t153)
    var t154 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t154)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source105 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit106 int = len(for_source105)
    var for_index107 int = 0
    Loop_loop168:
    for {
        var t169 bool = for_index107 < for_limit106
        if t169 {
            var for_item108 int32 = for_source105[for_index107]
            var t170 int = for_index107 + 1
            for_index107 = t170
            var value__16 int32 = for_item108
            var t171 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
            var t172 int32 = t171 + value__16
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t172)
            continue
        } else {
            break Loop_loop168
        }
    }
    var t156 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t156)
    var t157 FnIterator__int32 = countdown(4)
    var for_iter113 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t157)
    Loop_loop164:
    for {
        if true {
            var for_next114 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter113)
            switch for_next114.(type) {
            case Option__int32_None:
                break Loop_loop164
            case Option__int32_Some:
                var x115 int32 = for_next114.(Option__int32_Some)._0
                var value__17 int32 = x115
                var t167 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t167 {
                    break Loop_loop164
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop164
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter119 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop162:
    for {
        if true {
            var for_next120 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter119)
            switch for_next120.(type) {
            case Option__int_None:
                break Loop_loop162
            case Option__int_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop162
        }
    }
    var t160 FnIterator__int = _goml_m_range(3, 8)
    var t161 int = first_even(t160)
    println__T_int(t161)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv190 *ref_int32_x
    var t191 *ref_int32_x = ref__Ref_5int32(value__207)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv193 int32
    var t194 int32 = ref_get__Ref_5int32(self__208)
    retv193 = t194
    return retv193
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv198 FnIterator__int32
    var t199 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv198 = t199
    return retv198
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv201 FnIterator__int
    var t202 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv201 = t202
    return retv201
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv204 FnIterator__int
    retv204 = self__109
    return retv204
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv206 Option__int
    var t207 func() Option__int = self__102.next_fn
    var t208 Option__int = t207()
    retv206 = t208
    return retv206
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv210 bool
    var t211 bool = self__59 == other__60
    retv210 = t211
    return retv210
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv213 *_goml_vec_int32
    var t214 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv218 bool
    var t219 bool = self__65 == other__66
    retv218 = t219
    return retv218
}

func println__T_int32(value__1 int32) struct{} {
    var t221 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t221)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv224 *_goml_vec_Tuple2_5int32_6string
    var t225 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__126 *_goml_vec_Tuple2_5int32_6string, elem__127 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t229)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv232 string
    var t233 string = _goml_runtime_core_int32_to_string(self__6)
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv235 *ref_int_x
    var t236 *ref_int_x = ref__Ref_3int(value__207)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv238 int
    var t239 int = ref_get__Ref_3int(self__208)
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t243 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv246 []int32
    var t247 []int32 = self__175.items[start__176:end__177]
    retv246 = t247
    return retv246
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    var retv249 FnIterator__int32
    retv249 = self__109
    return retv249
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv251 Option__int32
    var t252 func() Option__int32 = self__102.next_fn
    var t253 Option__int32 = t252()
    retv251 = t253
    return retv251
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv255 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t256 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t257 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t256)
    })
    retv255 = t257
    return retv255
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv259 string
    var t260 string = _goml_runtime_core_int32_to_string(self__43)
    retv259 = t260
    return retv259
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv262 string
    retv262 = self__38
    return retv262
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv264 string
    var t265 string = _goml_runtime_core_int_to_string(self__40)
    retv264 = t265
    return retv264
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv267 FnIterator__int
    var t268 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv267 = t268
    return retv267
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env125 closure_env_countdown_0) Option__int32 {
    var retv276 Option__int32
    var current__1 *ref_int32_x = env125.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t279 bool = value__2 > 0
    var jp278 Option__int32
    if t279 {
        var t280 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t280)
        var t281 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp278 = t281
    } else {
        jp278 = Option__int32_None{}
    }
    retv276 = jp278
    return retv276
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env126 closure_env_goml_builtin_range_1) Option__int {
    var retv283 Option__int
    var current__220 *ref_int_x = env126.current_0
    var end__219 int = env126.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t286 bool = value__221 < end__219
    var jp285 Option__int
    if t286 {
        var t287 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t287)
        var t288 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp285 = t288
    } else {
        jp285 = Option__int_None{}
    }
    retv283 = jp285
    return retv283
}

func main() {
    main0()
}
