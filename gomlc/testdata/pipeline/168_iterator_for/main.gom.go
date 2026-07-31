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
    var retv212 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t213 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t214 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t213)
    })
    retv212 = t214
    return retv212
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var retv216 FnIterator__int
    var t217 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t218 int32 = t217 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t218)
    var t219 FnIterator__int = _goml_m_range(1, 5)
    retv216 = t219
    return retv216
}

func first_even(values__4 FnIterator__int) int {
    var retv221 int
    var for_iter154 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop223:
    for {
        if true {
            var for_next155 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter154)
            switch for_next155.(type) {
            case Option__int_None:
                break Loop_loop223
            case Option__int_Some:
                var x156 int = for_next155.(Option__int_Some)._0
                var value__5 int = x156
                var t226 int = value__5 / 2
                var t227 int = t226 * 2
                var t228 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t227, value__5)
                if t228 {
                    retv221 = value__5
                    return retv221
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop223
        }
    }
    retv221 = -1
    return retv221
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source162 *_goml_vec_int32 = values__6
    var for_limit163 int = vec_len__Vec_5int32(for_source162)
    var for_index164 int = 0
    Loop_loop266:
    for {
        var t267 bool = for_index164 < for_limit163
        if t267 {
            var for_item165 int32 = vec_get__Vec_5int32(for_source162, for_index164)
            var t268 int = for_index164 + 1
            for_index164 = t268
            var value__8 int32 = for_item165
            var t272 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
            if t272 {
                continue
            } else {
                var t270 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                var t271 int32 = t270 + value__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t271)
                continue
            }
        } else {
            break Loop_loop266
        }
    }
    var t231 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t231)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t232 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t232)
    var t233 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t233)
    var for_source173 *_goml_vec_Tuple2_5int32_6string = pairs__9
    var for_limit174 int = vec_len__Vec_21Tuple2_5int32_6string(for_source173)
    var for_index175 int = 0
    Loop_loop261:
    for {
        var t262 bool = for_index175 < for_limit174
        if t262 {
            var for_item176 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(for_source173, for_index175)
            var t263 int = for_index175 + 1
            for_index175 = t263
            var x178 int32 = for_item176._0
            var x179 string = for_item176._1
            var text__11 string = x179
            var number__10 int32 = x178
            var t264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
            var t265 string = t264 + text__11
            println__T_string(t265)
            continue
        } else {
            break Loop_loop261
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t235 FnIterator__int = counted_range(calls__12)
    var for_iter182 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t235)
    Loop_loop257:
    for {
        if true {
            var for_next183 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter182)
            switch for_next183.(type) {
            case Option__int_None:
                break Loop_loop257
            case Option__int_Some:
                var x184 int = for_next183.(Option__int_Some)._0
                var value__14 int = x184
                var t259 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
                var t260 int = t259 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t260)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop257
        }
    }
    var t237 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t237)
    var t238 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t238)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source189 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit190 int = len(for_source189)
    var for_index191 int = 0
    Loop_loop252:
    for {
        var t253 bool = for_index191 < for_limit190
        if t253 {
            var for_item192 int32 = for_source189[for_index191]
            var t254 int = for_index191 + 1
            for_index191 = t254
            var value__16 int32 = for_item192
            var t255 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
            var t256 int32 = t255 + value__16
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t256)
            continue
        } else {
            break Loop_loop252
        }
    }
    var t240 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t240)
    var t241 FnIterator__int32 = countdown(4)
    var for_iter197 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t241)
    Loop_loop248:
    for {
        if true {
            var for_next198 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter197)
            switch for_next198.(type) {
            case Option__int32_None:
                break Loop_loop248
            case Option__int32_Some:
                var x199 int32 = for_next198.(Option__int32_Some)._0
                var value__17 int32 = x199
                var t251 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t251 {
                    break Loop_loop248
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop248
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter203 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop246:
    for {
        if true {
            var for_next204 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter203)
            switch for_next204.(type) {
            case Option__int_None:
                break Loop_loop246
            case Option__int_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop246
        }
    }
    var t244 FnIterator__int = _goml_m_range(3, 8)
    var t245 int = first_even(t244)
    println__T_int(t245)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv274 *ref_int32_x
    var t275 *ref_int32_x = ref__Ref_5int32(value__207)
    retv274 = t275
    return retv274
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv277 int32
    var t278 int32 = ref_get__Ref_5int32(self__208)
    retv277 = t278
    return retv277
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv282 FnIterator__int32
    var t283 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv282 = t283
    return retv282
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv285 FnIterator__int
    var t286 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv285 = t286
    return retv285
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv288 FnIterator__int
    retv288 = self__109
    return retv288
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv290 Option__int
    var t291 func() Option__int = self__102.next_fn
    var t292 Option__int = t291()
    retv290 = t292
    return retv290
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv294 bool
    var t295 bool = self__59 == other__60
    retv294 = t295
    return retv294
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv297 *_goml_vec_int32
    var t298 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv297 = t298
    return retv297
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv302 bool
    var t303 bool = self__65 == other__66
    retv302 = t303
    return retv302
}

func println__T_int32(value__1 int32) struct{} {
    var t305 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t305)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv308 *_goml_vec_Tuple2_5int32_6string
    var t309 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv308 = t309
    return retv308
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__126 *_goml_vec_Tuple2_5int32_6string, elem__127 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t313)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv316 string
    var t317 string = _goml_runtime_core_int32_to_string(self__6)
    retv316 = t317
    return retv316
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv319 *ref_int_x
    var t320 *ref_int_x = ref__Ref_3int(value__207)
    retv319 = t320
    return retv319
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv322 int
    var t323 int = ref_get__Ref_3int(self__208)
    retv322 = t323
    return retv322
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t327 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t327)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv330 []int32
    var t331 []int32 = self__175.items[start__176:end__177]
    retv330 = t331
    return retv330
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    var retv333 FnIterator__int32
    retv333 = self__109
    return retv333
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv335 Option__int32
    var t336 func() Option__int32 = self__102.next_fn
    var t337 Option__int32 = t336()
    retv335 = t337
    return retv335
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv339 FnIterator__int
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t340 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t341 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t340)
    })
    retv339 = t341
    return retv339
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv343 string
    var t344 string = _goml_runtime_core_int32_to_string(self__43)
    retv343 = t344
    return retv343
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv346 string
    retv346 = self__38
    return retv346
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv348 string
    var t349 string = _goml_runtime_core_int_to_string(self__40)
    retv348 = t349
    return retv348
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv351 FnIterator__int
    var t352 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv351 = t352
    return retv351
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env209 closure_env_countdown_0) Option__int32 {
    var retv360 Option__int32
    var current__1 *ref_int32_x = env209.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t363 bool = value__2 > 0
    var jp362 Option__int32
    if t363 {
        var t364 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t364)
        var t365 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp362 = t365
    } else {
        jp362 = Option__int32_None{}
    }
    retv360 = jp362
    return retv360
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env210 closure_env_goml_builtin_range_1) Option__int {
    var retv367 Option__int
    var current__220 *ref_int_x = env210.current_0
    var end__219 int = env210.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t370 bool = value__221 < end__219
    var jp369 Option__int
    if t370 {
        var t371 int = value__221 + 1
        ref_set__Ref_3int(current__220, t371)
        var t372 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp369 = t372
    } else {
        jp369 = Option__int_None{}
    }
    retv367 = jp369
    return retv367
}

func main() {
    main0()
}
