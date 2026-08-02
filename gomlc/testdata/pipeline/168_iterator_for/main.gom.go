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
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t216 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t217 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t216)
    })
    return t217
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var t220 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t221 int32 = t220 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t221)
    var t222 FnIterator__int = _goml_m_range(1, 5)
    return t222
}

func first_even(values__4 FnIterator__int) int {
    var for_iter157 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop226:
    for {
        var for_next158 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter157)
        switch for_next158.(type) {
        case Option__int_None:
            break Loop_loop226
        case Option__int_Some:
            var x159 int = for_next158.(Option__int_Some)._0
            var t229 int = x159 / 2
            var t230 int = t229 * 2
            var t231 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t230, x159)
            if t231 {
                return x159
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return -1
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_limit166 int = vec_len__Vec_5int32(values__6)
    var for_index167 int = 0
    Loop_loop269:
    for {
        var t270 bool = for_index167 < for_limit166
        if t270 {
            var for_item168 int32 = vec_get__Vec_5int32(values__6, for_index167)
            var t271 int = for_index167 + 1
            for_index167 = t271
            var t275 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(for_item168, 20)
            if t275 {
                continue
            } else {
                var t273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                var t274 int32 = t273 + for_item168
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t274)
                continue
            }
        } else {
            break Loop_loop269
        }
    }
    var t234 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t234)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t235 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t235)
    var t236 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t236)
    var for_limit177 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index178 int = 0
    Loop_loop264:
    for {
        var t265 bool = for_index178 < for_limit177
        if t265 {
            var for_item179 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index178)
            var t266 int = for_index178 + 1
            for_index178 = t266
            var x181 int32 = for_item179._0
            var x182 string = for_item179._1
            var t267 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x181)
            var t268 string = t267 + x182
            println__T_string(t268)
            continue
        } else {
            break Loop_loop264
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t238 FnIterator__int = counted_range(calls__12)
    var for_iter185 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t238)
    Loop_loop260:
    for {
        var for_next186 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter185)
        switch for_next186.(type) {
        case Option__int_None:
            break Loop_loop260
        case Option__int_Some:
            var x187 int = for_next186.(Option__int_Some)._0
            var t262 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
            var t263 int = t262 + x187
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t263)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t240 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t240)
    var t241 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t241)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source192 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit193 int = len(for_source192)
    var for_index194 int = 0
    Loop_loop255:
    for {
        var t256 bool = for_index194 < for_limit193
        if t256 {
            var for_item195 int32 = for_source192[for_index194]
            var t257 int = for_index194 + 1
            for_index194 = t257
            var t258 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
            var t259 int32 = t258 + for_item195
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t259)
            continue
        } else {
            break Loop_loop255
        }
    }
    var t243 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t243)
    var t244 FnIterator__int32 = countdown(4)
    var for_iter200 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t244)
    Loop_loop251:
    for {
        var for_next201 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter200)
        switch for_next201.(type) {
        case Option__int32_None:
            break Loop_loop251
        case Option__int32_Some:
            var x202 int32 = for_next201.(Option__int32_Some)._0
            var t254 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x202, 2)
            if t254 {
                break Loop_loop251
            } else {
                println__T_int32(x202)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter206 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop249:
    for {
        var for_next207 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter206)
        switch for_next207.(type) {
        case Option__int_None:
            break Loop_loop249
        case Option__int_Some:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t247 FnIterator__int = _goml_m_range(3, 8)
    var t248 int = first_even(t247)
    println__T_int(t248)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t278 *ref_int32_x = ref__Ref_5int32(value__207)
    return t278
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t281 int32 = ref_get__Ref_5int32(self__208)
    return t281
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var t286 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    return t286
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var t289 FnIterator__int = __goml_builtin_range(start__222, end__223)
    return t289
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    return self__109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var t294 func() Option__int = self__102.next_fn
    var t295 Option__int = t294()
    return t295
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t298 bool = self__59 == other__60
    return t298
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t301 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t301
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t306 bool = self__65 == other__66
    return t306
}

func println__T_int32(value__1 int32) struct{} {
    var t308 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t308)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t312 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t312
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__126 *_goml_vec_Tuple2_5int32_6string, elem__127 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t316)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t320 string = _goml_runtime_core_int32_to_string(self__6)
    return t320
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t323 *ref_int_x = ref__Ref_3int(value__207)
    return t323
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t326 int = ref_get__Ref_3int(self__208)
    return t326
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t330 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t330)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var t334 []int32 = self__175.items[start__176:end__177]
    return t334
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    return self__109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var t339 func() Option__int32 = self__102.next_fn
    var t340 Option__int32 = t339()
    return t340
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t343 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t344 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t343)
    })
    return t344
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t347 string = _goml_runtime_core_int32_to_string(self__43)
    return t347
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t352 string = _goml_runtime_core_int_to_string(self__40)
    return t352
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var t355 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    return t355
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env212 closure_env_countdown_0) Option__int32 {
    var current__1 *ref_int32_x = env212.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t366 bool = value__2 > 0
    if t366 {
        var t367 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t367)
        var t368 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        return t368
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env213 closure_env_goml_builtin_range_1) Option__int {
    var current__220 *ref_int_x = env213.current_0
    var end__219 int = env213.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t373 bool = value__221 < end__219
    if t373 {
        var t374 int = value__221 + 1
        ref_set__Ref_3int(current__220, t374)
        var t375 Option__int = Option__int_Some{
            _0: value__221,
        }
        return t375
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
