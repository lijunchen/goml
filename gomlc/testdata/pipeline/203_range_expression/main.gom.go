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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_inclusive_0 struct {
    finished_0 *ref_bool_x
    current_1 *ref_int_x
    end_2 int
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func _goml_m_range(start__0 int32, end__1 int32) int32 {
    var retv190 int32
    var t191 int32 = start__0 + end__1
    retv190 = t191
    return retv190
}

func endpoint(calls__2 *ref_int_x, value__3 int) int {
    var retv193 int
    var t194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__2)
    var t195 int = t194 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__2, t195)
    retv193 = value__3
    return retv193
}

func main0() struct{} {
    var for_index156 int = 1
    var for_limit157 int = 4
    Loop_loop227:
    for {
        var t228 bool = for_index156 < for_limit157
        if t228 {
            var for_item158 int = for_index156
            var t229 int = for_index156 + 1
            for_index156 = t229
            var value__4 int = for_item158
            println__T_int(value__4)
            continue
        } else {
            break Loop_loop227
        }
    }
    var calls__5 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index162 int = endpoint(calls__5, 4)
    var for_limit163 int = endpoint(calls__5, 6)
    var for_done164 bool = for_index162 > for_limit163
    Loop_loop220:
    for {
        var t221 bool = !for_done164
        if t221 {
            var for_item165 int = for_index162
            var t223 bool = for_index162 == for_limit163
            if t223 {
                for_done164 = true
            } else {
                var t225 int = for_index162 + 1
                for_index162 = t225
            }
            var value__6 int = for_item165
            println__T_int(value__6)
            continue
        } else {
            break Loop_loop220
        }
    }
    var for_index169 int = 3
    var for_limit170 int = 1
    var for_done171 bool = for_index169 > for_limit170
    Loop_loop213:
    for {
        var t214 bool = !for_done171
        if t214 {
            var for_item172 int = for_index169
            var t216 bool = for_index169 == for_limit170
            if t216 {
                for_done171 = true
            } else {
                var t218 int = for_index169 + 1
                for_index169 = t218
            }
            var value__7 int = for_item172
            println__T_int(value__7)
            continue
        } else {
            break Loop_loop213
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index176 int = maximum__8
    var for_limit177 int = maximum__8
    var for_done178 bool = for_index176 > for_limit177
    Loop_loop206:
    for {
        var t207 bool = !for_done178
        if t207 {
            var for_item179 int = for_index176
            var t209 bool = for_index176 == for_limit177
            if t209 {
                for_done178 = true
            } else {
                var t211 int = for_index176 + 1
                for_index176 = t211
            }
            var value__9 int = for_item179
            println__T_int(value__9)
            continue
        } else {
            break Loop_loop206
        }
    }
    var iterator__10 FnIterator__int = __goml_builtin_range_inclusive(8, 8)
    var mtmp183 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__10)
    switch mtmp183.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var x184 int = mtmp183.(Some)._0
        var value__11 int = x184
        println__T_int(value__11)
    default:
        panic("non-exhaustive match")
    }
    var t202 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    println__T_int(t202)
    var t203 int32 = _goml_m_range(10, 20)
    println__T_int32(t203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv231 int
    var t232 int = ref_get__Ref_3int(self__208)
    retv231 = t232
    return retv231
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t236 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t236)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv239 *ref_int_x
    var t240 *ref_int_x = ref__Ref_3int(value__207)
    retv239 = t240
    return retv239
}

func __goml_builtin_range_inclusive(start__280 int, end__281 int) FnIterator__int {
    var retv242 FnIterator__int
    var current__282 *ref_int_x = ref__Ref_3int(start__280)
    var finished__283 *ref_bool_x = ref__Ref_4bool(false)
    var t243 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: finished__283,
        current_1: current__282,
        end_2: end__281,
    }
    var t244 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(t243)
    })
    retv242 = t244
    return retv242
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv246 Option__int
    var t247 func() Option__int = self__102.next_fn
    var t248 Option__int = t247()
    retv246 = t248
    return retv246
}

func println__T_string(value__1 string) struct{} {
    var t250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t253)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv256 string
    var t257 string = _goml_runtime_core_int_to_string(self__40)
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv259 bool
    var t260 bool = self__59 == other__60
    retv259 = t260
    return retv259
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv262 FnIterator__int
    var t263 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv262 = t263
    return retv262
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv265 string
    retv265 = self__38
    return retv265
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv267 string
    var t268 string = _goml_runtime_core_int32_to_string(self__43)
    retv267 = t268
    return retv267
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env188 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var retv280 Option__int
    var finished__283 *ref_bool_x = env188.finished_0
    var current__282 *ref_int_x = env188.current_1
    var end__281 int = env188.end_2
    var t289 bool = ref_get__Ref_4bool(finished__283)
    var jp284 bool
    if t289 {
        jp284 = true
    } else {
        var t290 int = ref_get__Ref_3int(current__282)
        var t291 bool = t290 > end__281
        jp284 = t291
    }
    var jp282 Option__int
    if jp284 {
        jp282 = None{}
    } else {
        var value__284 int = ref_get__Ref_3int(current__282)
        var t287 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(value__284, end__281)
        if t287 {
            ref_set__Ref_4bool(finished__283, true)
        } else {
            var t288 int = value__284 + 1
            ref_set__Ref_3int(current__282, t288)
        }
        var t286 Option__int = Some{
            _0: value__284,
        }
        jp282 = t286
    }
    retv280 = jp282
    return retv280
}

func main() {
    main0()
}
