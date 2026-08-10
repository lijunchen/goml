package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
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

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
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
    var value__0 string = "a你好z"
    var t184 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline368 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t184)
    _goml_runtime_core_string_println(inline368)
    var t185 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline365 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline365)
    var t186 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline362 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t186)
    _goml_runtime_core_string_println(inline362)
    var t187 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline359 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t187)
    _goml_runtime_core_string_println(inline359)
    var t188 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline356 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t188)
    _goml_runtime_core_string_println(inline356)
    var t189 bool
    var inline344 string = ""
    var inline345 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline344)
    var inline346 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline347 bool = inline345 > inline346
    if inline347 {
        t189 = false
    } else {
        var inline348 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline349 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline344)
        var inline350 int = inline348 - inline349
        var inline351 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline350)
        if inline351 {
            var inline352 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline353 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline350, inline352)
            var inline354 bool = inline353 == inline344
            t189 = inline354
        } else {
            t189 = false
        }
    }
    var inline341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    _goml_runtime_core_string_println(inline341)
    var t190 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline338 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline338)
    var t191 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline335 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline335)
    var t192 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline332 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    _goml_runtime_core_string_println(inline332)
    var t193 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline329 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    _goml_runtime_core_string_println(inline329)
    var t194 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline326 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t194)
    _goml_runtime_core_string_println(inline326)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__271 string, prefix__272 string) bool {
    var t208 int
    var inline384 int = _goml_runtime_core_string_len(prefix__272)
    t208 = inline384
    var t209 int
    var inline382 int = _goml_runtime_core_string_len(self__271)
    t209 = inline382
    var t210 bool = t208 <= t209
    var jp204 bool
    if t210 {
        var t211 int
        var inline375 int = _goml_runtime_core_string_len(prefix__272)
        t211 = inline375
        var inline373 bool = string_is_char_boundary(self__271, t211)
        jp204 = inline373
    } else {
        jp204 = false
    }
    if jp204 {
        var t205 int
        var inline380 int = _goml_runtime_core_string_len(prefix__272)
        t205 = inline380
        var t206 string
        var inline377 int = 0
        var inline378 string = string_byte_slice(self__271, inline377, t205)
        t206 = inline378
        var t207 bool = t206 == prefix__272
        return t207
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__273 string, suffix__274 string) bool {
    var t217 int
    var inline398 int = _goml_runtime_core_string_len(suffix__274)
    t217 = inline398
    var t218 int
    var inline396 int = _goml_runtime_core_string_len(self__273)
    t218 = inline396
    var t219 bool = t217 > t218
    if t219 {
        return false
    } else {
        var t220 int
        var inline394 int = _goml_runtime_core_string_len(self__273)
        t220 = inline394
        var t221 int
        var inline392 int = _goml_runtime_core_string_len(suffix__274)
        t221 = inline392
        var start__275 int = t220 - t221
        var t224 bool
        var inline390 bool = string_is_char_boundary(self__273, start__275)
        t224 = inline390
        if t224 {
            var t225 int
            var inline388 int = _goml_runtime_core_string_len(self__273)
            t225 = inline388
            var t226 string
            var inline386 string = string_byte_slice(self__273, start__275, t225)
            t226 = inline386
            var t227 bool = t226 == suffix__274
            return t227
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__276 string, expected__277 string) bool {
    var t232 int
    var inline426 int = _goml_runtime_core_string_len(expected__277)
    t232 = inline426
    var t233 bool = t232 == 0
    if t233 {
        return true
    } else {
        var t236 int
        var inline424 int = _goml_runtime_core_string_len(expected__277)
        t236 = inline424
        var t237 int
        var inline422 int = _goml_runtime_core_string_len(self__276)
        t237 = inline422
        var t238 bool = t236 > t237
        if t238 {
            return false
        } else {
            var t239 int
            var inline420 int = _goml_runtime_core_string_len(self__276)
            t239 = inline420
            var t240 int
            var inline418 int = _goml_runtime_core_string_len(expected__277)
            t240 = inline418
            var t241 int = t239 - t240
            var t242 int = t241 + 1
            var t243 FnIterator__int
            var inline412 int = 0
            var inline413 *ref_int_x = ref__Ref_3int(inline412)
            var inline414 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline413,
                end_1: t242,
            }
            var inline415 func() Option__int = func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline414)
            }
            var inline416 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline415)
            t243 = inline416
            var for_iter146 FnIterator__int
            for_iter146 = t243
            Loop_loop245:
            for {
                var for_next147 Option__int
                var inline408 func() Option__int = for_iter146.next_fn
                var inline409 Option__int = inline408()
                for_next147 = inline409
                switch for_next147.(type) {
                case None:
                    break Loop_loop245
                case Some:
                    var x148 int = for_next147.(Some)._0
                    var t247 int
                    var inline406 int = _goml_runtime_core_string_len(expected__277)
                    t247 = inline406
                    var end__279 int = x148 + t247
                    var t255 bool
                    var inline404 bool = string_is_char_boundary(self__276, x148)
                    t255 = inline404
                    var jp252 bool
                    if t255 {
                        var inline400 bool = string_is_char_boundary(self__276, end__279)
                        jp252 = inline400
                    } else {
                        jp252 = false
                    }
                    var jp250 bool
                    if jp252 {
                        var t253 string
                        var inline402 string = string_byte_slice(self__276, x148, end__279)
                        t253 = inline402
                        var t254 bool = t253 == expected__277
                        jp250 = t254
                    } else {
                        jp250 = false
                    }
                    if jp250 {
                        return true
                    } else {
                        continue
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
            return false
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t259 string = _goml_runtime_core_bool_to_string(self__64)
    return t259
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t262 int = _goml_runtime_core_string_len(self__36)
    return t262
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t265 bool = string_is_char_boundary(self__44, index__45)
    return t265
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline428 bool = string_is_char_boundary(self__41, start__42)
    var inline430 bool
    if inline428 {
        var inline433 bool = string_is_char_boundary(self__41, end__43)
        inline430 = inline433
    } else {
        inline430 = false
    }
    if inline430 {
        var inline431 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline431
    } else {
        var inline432 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline432
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t293 bool = index__16 < 0
    var jp285 bool
    if t293 {
        jp285 = true
    } else {
        var t294 int
        var inline437 int = _goml_runtime_core_string_len(value__15)
        t294 = inline437
        var t295 bool = index__16 > t294
        jp285 = t295
    }
    if jp285 {
        return false
    } else {
        var t288 int
        var inline441 int = _goml_runtime_core_string_len(value__15)
        t288 = inline441
        var t289 bool = index__16 == t288
        if t289 {
            return true
        } else {
            var t290 uint8
            var inline439 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t290 = inline439
            var t291_rhs uint8 = 192
            var t291 uint8 = t290 & t291_rhs
            var t292 bool = t291 != 128
            return t292
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t304 bool = string_is_char_boundary(value__21, start__22)
    var jp301 bool
    if t304 {
        var t305 bool = string_is_char_boundary(value__21, end__23)
        jp301 = t305
    } else {
        jp301 = false
    }
    if jp301 {
        var t302 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t302
    } else {
        var t303 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t303
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__170 func() Option__int) FnIterator__int {
    var t308 FnIterator__int = FnIterator__int{
        next_fn: next_fn__170,
    }
    return t308
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env182 closure_env_goml_builtin_range_0) Option__int {
    var current__320 *ref_int_x = env182.current_0
    var end__319 int = env182.end_1
    var value__321 int = ref_get__Ref_3int(current__320)
    var t322 bool = value__321 < end__319
    if t322 {
        var t323 int = value__321 + 1
        ref_set__Ref_3int(current__320, t323)
        var t324 Option__int = Some{
            _0: value__321,
        }
        return t324
    } else {
        return None{}
    }
}

func main() {
    main0()
}
