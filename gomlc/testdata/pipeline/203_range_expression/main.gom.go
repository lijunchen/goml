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
            var inline297 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item158)
            _goml_runtime_core_string_println(inline297)
            continue
        } else {
            break Loop_loop227
        }
    }
    var calls__5 *ref_int_x
    var inline348 int = 0
    var inline349 *ref_int_x = ref__Ref_3int(inline348)
    calls__5 = inline349
    var for_index162 int
    var inline343 int = 4
    var inline344 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline345 int = inline344 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline345)
    for_index162 = inline343
    var for_limit163 int
    var inline338 int = 6
    var inline339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline340 int = inline339 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline340)
    for_limit163 = inline338
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
            var inline300 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item165)
            _goml_runtime_core_string_println(inline300)
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
            var inline303 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item172)
            _goml_runtime_core_string_println(inline303)
            continue
        } else {
            break Loop_loop213
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index176 int = maximum__8
    var for_done178 bool = for_index176 > maximum__8
    Loop_loop206:
    for {
        var t207 bool = !for_done178
        if t207 {
            var for_item179 int = for_index176
            var t209 bool = for_index176 == maximum__8
            if t209 {
                for_done178 = true
            } else {
                var t211 int = for_index176 + 1
                for_index176 = t211
            }
            var inline306 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item179)
            _goml_runtime_core_string_println(inline306)
            continue
        } else {
            break Loop_loop206
        }
    }
    var iterator__10 FnIterator__int
    var inline331 int = 8
    var inline332 int = 8
    var inline333 *ref_int_x = ref__Ref_3int(inline331)
    var inline334 *ref_bool_x = ref__Ref_4bool(false)
    var inline335 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline334,
        current_1: inline333,
        end_2: inline332,
    }
    var inline336 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline335)
    })
    iterator__10 = inline336
    var mtmp183 Option__int
    var inline328 func() Option__int = iterator__10.next_fn
    var inline329 Option__int = inline328()
    mtmp183 = inline329
    switch mtmp183.(type) {
    case None:
        var inline309 string = "missing"
        var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline309)
        _goml_runtime_core_string_println(inline310)
    case Some:
        var x184 int = mtmp183.(Some)._0
        var inline313 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
        _goml_runtime_core_string_println(inline313)
    default:
        panic("non-exhaustive match")
    }
    var t202 int
    var inline326 int = ref_get__Ref_3int(calls__5)
    t202 = inline326
    var inline323 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t202)
    _goml_runtime_core_string_println(inline323)
    var t203 int32
    var inline319 int32 = 10
    var inline320 int32 = 20
    var inline321 int32 = inline319 + inline320
    t203 = inline321
    var inline316 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
    _goml_runtime_core_string_println(inline316)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t232 int = ref_get__Ref_3int(self__208)
    return t232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t257 string = _goml_runtime_core_int_to_string(self__40)
    return t257
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var t263 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    return t263
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t268 string = _goml_runtime_core_int32_to_string(self__43)
    return t268
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env188 closure_env_goml_builtin_range_inclusive_0) Option__int {
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
    if jp284 {
        return None{}
    } else {
        var value__284 int = ref_get__Ref_3int(current__282)
        var t287 bool
        var inline356 bool = value__284 == end__281
        t287 = inline356
        if t287 {
            ref_set__Ref_4bool(finished__283, true)
        } else {
            var t288 int = value__284 + 1
            ref_set__Ref_3int(current__282, t288)
        }
        var t286 Option__int = Some{
            _0: value__284,
        }
        return t286
    }
}

func main() {
    main0()
}
