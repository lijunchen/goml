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
    var retv168 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t169 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t170 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t169)
    })
    retv168 = t170
    return retv168
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var retv172 FnIterator__int
    var t173 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t174 int32 = t173 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t174)
    var t175 FnIterator__int = _goml_m_range(1, 5)
    retv172 = t175
    return retv172
}

func first_even(values__4 FnIterator__int) int {
    var retv177 int
    var for_iter110 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop179:
    for {
        if true {
            var for_next111 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter110)
            switch for_next111.(type) {
            case Option__int_None:
                break Loop_loop179
            case Option__int_Some:
                var x112 int = for_next111.(Option__int_Some)._0
                var value__5 int = x112
                var t182 int = value__5 / 2
                var t183 int = t182 * 2
                var t184 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t183, value__5)
                if t184 {
                    retv177 = value__5
                    return retv177
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop179
        }
    }
    retv177 = -1
    return retv177
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source118 *_goml_vec_int32 = values__6
    var for_limit119 int = vec_len__Vec_5int32(for_source118)
    var for_index120 int = 0
    Loop_loop222:
    for {
        var t223 bool = for_index120 < for_limit119
        if t223 {
            var for_item121 int32 = vec_get__Vec_5int32(for_source118, for_index120)
            var t224 int = for_index120 + 1
            for_index120 = t224
            var value__8 int32 = for_item121
            var t228 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
            if t228 {
                continue
            } else {
                var t226 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                var t227 int32 = t226 + value__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t227)
                continue
            }
        } else {
            break Loop_loop222
        }
    }
    var t187 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t187)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t188 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t188)
    var t189 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t189)
    var for_source129 *_goml_vec_Tuple2_5int32_6string = pairs__9
    var for_limit130 int = vec_len__Vec_21Tuple2_5int32_6string(for_source129)
    var for_index131 int = 0
    Loop_loop217:
    for {
        var t218 bool = for_index131 < for_limit130
        if t218 {
            var for_item132 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(for_source129, for_index131)
            var t219 int = for_index131 + 1
            for_index131 = t219
            var x134 int32 = for_item132._0
            var x135 string = for_item132._1
            var text__11 string = x135
            var number__10 int32 = x134
            var t220 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
            var t221 string = t220 + text__11
            println__T_string(t221)
            continue
        } else {
            break Loop_loop217
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t191 FnIterator__int = counted_range(calls__12)
    var for_iter138 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t191)
    Loop_loop213:
    for {
        if true {
            var for_next139 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter138)
            switch for_next139.(type) {
            case Option__int_None:
                break Loop_loop213
            case Option__int_Some:
                var x140 int = for_next139.(Option__int_Some)._0
                var value__14 int = x140
                var t215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
                var t216 int = t215 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t216)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop213
        }
    }
    var t193 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t193)
    var t194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t194)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source145 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit146 int = len(for_source145)
    var for_index147 int = 0
    Loop_loop208:
    for {
        var t209 bool = for_index147 < for_limit146
        if t209 {
            var for_item148 int32 = for_source145[for_index147]
            var t210 int = for_index147 + 1
            for_index147 = t210
            var value__16 int32 = for_item148
            var t211 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
            var t212 int32 = t211 + value__16
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t212)
            continue
        } else {
            break Loop_loop208
        }
    }
    var t196 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t196)
    var t197 FnIterator__int32 = countdown(4)
    var for_iter153 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t197)
    Loop_loop204:
    for {
        if true {
            var for_next154 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter153)
            switch for_next154.(type) {
            case Option__int32_None:
                break Loop_loop204
            case Option__int32_Some:
                var x155 int32 = for_next154.(Option__int32_Some)._0
                var value__17 int32 = x155
                var t207 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t207 {
                    break Loop_loop204
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop204
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter159 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop202:
    for {
        if true {
            var for_next160 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter159)
            switch for_next160.(type) {
            case Option__int_None:
                break Loop_loop202
            case Option__int_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop202
        }
    }
    var t200 FnIterator__int = _goml_m_range(3, 8)
    var t201 int = first_even(t200)
    println__T_int(t201)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv230 *ref_int32_x
    var t231 *ref_int32_x = ref__Ref_5int32(value__207)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv233 int32
    var t234 int32 = ref_get__Ref_5int32(self__208)
    retv233 = t234
    return retv233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv238 FnIterator__int32
    var t239 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv238 = t239
    return retv238
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv241 FnIterator__int
    var t242 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv241 = t242
    return retv241
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv244 FnIterator__int
    retv244 = self__109
    return retv244
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv246 Option__int
    var t247 func() Option__int = self__102.next_fn
    var t248 Option__int = t247()
    retv246 = t248
    return retv246
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv250 bool
    var t251 bool = self__59 == other__60
    retv250 = t251
    return retv250
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv253 *_goml_vec_int32
    var t254 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv253 = t254
    return retv253
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv258 bool
    var t259 bool = self__65 == other__66
    retv258 = t259
    return retv258
}

func println__T_int32(value__1 int32) struct{} {
    var t261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t261)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv264 *_goml_vec_Tuple2_5int32_6string
    var t265 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv264 = t265
    return retv264
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__126 *_goml_vec_Tuple2_5int32_6string, elem__127 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t269)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv272 string
    var t273 string = _goml_runtime_core_int32_to_string(self__6)
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv275 *ref_int_x
    var t276 *ref_int_x = ref__Ref_3int(value__207)
    retv275 = t276
    return retv275
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv278 int
    var t279 int = ref_get__Ref_3int(self__208)
    retv278 = t279
    return retv278
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t283 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t283)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv286 []int32
    var t287 []int32 = self__175.items[start__176:end__177]
    retv286 = t287
    return retv286
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    var retv289 FnIterator__int32
    retv289 = self__109
    return retv289
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv291 Option__int32
    var t292 func() Option__int32 = self__102.next_fn
    var t293 Option__int32 = t292()
    retv291 = t293
    return retv291
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv295 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t296 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t297 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t296)
    })
    retv295 = t297
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv299 string
    var t300 string = _goml_runtime_core_int32_to_string(self__43)
    retv299 = t300
    return retv299
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv302 string
    retv302 = self__38
    return retv302
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv304 string
    var t305 string = _goml_runtime_core_int_to_string(self__40)
    retv304 = t305
    return retv304
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv307 FnIterator__int
    var t308 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv307 = t308
    return retv307
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env165 closure_env_countdown_0) Option__int32 {
    var retv316 Option__int32
    var current__1 *ref_int32_x = env165.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t319 bool = value__2 > 0
    var jp318 Option__int32
    if t319 {
        var t320 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t320)
        var t321 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp318 = t321
    } else {
        jp318 = Option__int32_None{}
    }
    retv316 = jp318
    return retv316
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env166 closure_env_goml_builtin_range_1) Option__int {
    var retv323 Option__int
    var current__220 *ref_int_x = env166.current_0
    var end__219 int = env166.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t326 bool = value__221 < end__219
    var jp325 Option__int
    if t326 {
        var t327 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t327)
        var t328 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp325 = t328
    } else {
        jp325 = Option__int_None{}
    }
    retv323 = jp325
    return retv323
}

func main() {
    main0()
}
