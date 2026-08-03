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
    var for_index137 int = 1
    var for_limit138 int = 4
    Loop_loop208:
    for {
        var t209 bool = for_index137 < for_limit138
        if t209 {
            var for_item139 int = for_index137
            var t210 int = for_index137 + 1
            for_index137 = t210
            var inline278 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item139)
            _goml_runtime_core_string_println(inline278)
            continue
        } else {
            break Loop_loop208
        }
    }
    var calls__5 *ref_int_x
    var inline329 int = 0
    var inline330 *ref_int_x = ref__Ref_3int(inline329)
    calls__5 = inline330
    var for_index143 int
    var inline324 int = 4
    var inline325 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline326 int = inline325 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline326)
    for_index143 = inline324
    var for_limit144 int
    var inline319 int = 6
    var inline320 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(calls__5)
    var inline321 int = inline320 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(calls__5, inline321)
    for_limit144 = inline319
    var for_done145 bool = for_index143 > for_limit144
    Loop_loop201:
    for {
        var t202 bool = !for_done145
        if t202 {
            var for_item146 int = for_index143
            var t204 bool = for_index143 == for_limit144
            if t204 {
                for_done145 = true
            } else {
                var t206 int = for_index143 + 1
                for_index143 = t206
            }
            var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item146)
            _goml_runtime_core_string_println(inline281)
            continue
        } else {
            break Loop_loop201
        }
    }
    var for_index150 int = 3
    var for_limit151 int = 1
    var for_done152 bool = for_index150 > for_limit151
    Loop_loop194:
    for {
        var t195 bool = !for_done152
        if t195 {
            var for_item153 int = for_index150
            var t197 bool = for_index150 == for_limit151
            if t197 {
                for_done152 = true
            } else {
                var t199 int = for_index150 + 1
                for_index150 = t199
            }
            var inline284 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item153)
            _goml_runtime_core_string_println(inline284)
            continue
        } else {
            break Loop_loop194
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index157 int = maximum__8
    var for_done159 bool = for_index157 > maximum__8
    Loop_loop187:
    for {
        var t188 bool = !for_done159
        if t188 {
            var for_item160 int = for_index157
            var t190 bool = for_index157 == maximum__8
            if t190 {
                for_done159 = true
            } else {
                var t192 int = for_index157 + 1
                for_index157 = t192
            }
            var inline287 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(for_item160)
            _goml_runtime_core_string_println(inline287)
            continue
        } else {
            break Loop_loop187
        }
    }
    var iterator__10 FnIterator__int
    var inline312 int = 8
    var inline313 int = 8
    var inline314 *ref_int_x = ref__Ref_3int(inline312)
    var inline315 *ref_bool_x = ref__Ref_4bool(false)
    var inline316 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline315,
        current_1: inline314,
        end_2: inline313,
    }
    var inline317 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline316)
    })
    iterator__10 = inline317
    var mtmp164 Option__int
    var inline309 func() Option__int = iterator__10.next_fn
    var inline310 Option__int = inline309()
    mtmp164 = inline310
    switch mtmp164.(type) {
    case None:
        var inline290 string = "missing"
        var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline290)
        _goml_runtime_core_string_println(inline291)
    case Some:
        var x165 int = mtmp164.(Some)._0
        var inline294 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x165)
        _goml_runtime_core_string_println(inline294)
    default:
        panic("non-exhaustive match")
    }
    var t183 int
    var inline307 int = ref_get__Ref_3int(calls__5)
    t183 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t183)
    _goml_runtime_core_string_println(inline304)
    var t184 int32
    var inline300 int32 = 10
    var inline301 int32 = 20
    var inline302 int32 = inline300 + inline301
    t184 = inline302
    var inline297 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t184)
    _goml_runtime_core_string_println(inline297)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__216 *ref_int_x) int {
    var t213 int = ref_get__Ref_3int(self__216)
    return t213
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__217 *ref_int_x, value__218 int) struct{} {
    ref_set__Ref_3int(self__217, value__218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t238 string = _goml_runtime_core_int_to_string(self__69)
    return t238
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t244 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t244
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t249 string = _goml_runtime_core_int32_to_string(self__72)
    return t249
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env169 closure_env_goml_builtin_range_inclusive_0) Option__int {
    var finished__265 *ref_bool_x = env169.finished_0
    var current__264 *ref_int_x = env169.current_1
    var end__263 int = env169.end_2
    var t270 bool = ref_get__Ref_4bool(finished__265)
    var jp265 bool
    if t270 {
        jp265 = true
    } else {
        var t271 int = ref_get__Ref_3int(current__264)
        var t272 bool = t271 > end__263
        jp265 = t272
    }
    if jp265 {
        return None{}
    } else {
        var value__266 int = ref_get__Ref_3int(current__264)
        var t268 bool
        var inline337 bool = value__266 == end__263
        t268 = inline337
        if t268 {
            ref_set__Ref_4bool(finished__265, true)
        } else {
            var t269 int = value__266 + 1
            ref_set__Ref_3int(current__264, t269)
        }
        var t267 Option__int = Some{
            _0: value__266,
        }
        return t267
    }
}

func main() {
    main0()
}
