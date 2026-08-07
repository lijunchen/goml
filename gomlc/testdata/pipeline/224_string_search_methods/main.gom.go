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
    var inline374 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t184)
    _goml_runtime_core_string_println(inline374)
    var t185 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline371 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline371)
    var t186 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline368 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t186)
    _goml_runtime_core_string_println(inline368)
    var t187 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline365 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t187)
    _goml_runtime_core_string_println(inline365)
    var t188 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline362 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t188)
    _goml_runtime_core_string_println(inline362)
    var t189 bool
    var inline350 string = ""
    var inline351 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline350)
    var inline352 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline353 bool = inline351 > inline352
    if inline353 {
        t189 = false
    } else {
        var inline354 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline355 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline350)
        var inline356 int = inline354 - inline355
        var inline357 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline356)
        if inline357 {
            var inline358 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline359 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline356, inline358)
            var inline360 bool = _goml_m_trait__impl_i_PartialEq_i_string_i_eq(inline359, inline350)
            t189 = inline360
        } else {
            t189 = false
        }
    }
    var inline347 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    _goml_runtime_core_string_println(inline347)
    var t190 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline344 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline344)
    var t191 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline341)
    var t192 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline338 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    _goml_runtime_core_string_println(inline338)
    var t193 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline335 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    _goml_runtime_core_string_println(inline335)
    var t194 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline332 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t194)
    _goml_runtime_core_string_println(inline332)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__279 string, prefix__280 string) bool {
    var t208 int
    var inline392 int = _goml_runtime_core_string_len(prefix__280)
    t208 = inline392
    var t209 int
    var inline390 int = _goml_runtime_core_string_len(self__279)
    t209 = inline390
    var t210 bool = t208 <= t209
    var jp204 bool
    if t210 {
        var t211 int
        var inline381 int = _goml_runtime_core_string_len(prefix__280)
        t211 = inline381
        var inline379 bool = string_is_char_boundary(self__279, t211)
        jp204 = inline379
    } else {
        jp204 = false
    }
    if jp204 {
        var t205 int
        var inline388 int = _goml_runtime_core_string_len(prefix__280)
        t205 = inline388
        var t206 string
        var inline385 int = 0
        var inline386 string = string_byte_slice(self__279, inline385, t205)
        t206 = inline386
        var inline383 bool = t206 == prefix__280
        return inline383
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__281 string, suffix__282 string) bool {
    var t217 int
    var inline408 int = _goml_runtime_core_string_len(suffix__282)
    t217 = inline408
    var t218 int
    var inline406 int = _goml_runtime_core_string_len(self__281)
    t218 = inline406
    var t219 bool = t217 > t218
    if t219 {
        return false
    } else {
        var t220 int
        var inline404 int = _goml_runtime_core_string_len(self__281)
        t220 = inline404
        var t221 int
        var inline402 int = _goml_runtime_core_string_len(suffix__282)
        t221 = inline402
        var start__283 int = t220 - t221
        var t224 bool
        var inline400 bool = string_is_char_boundary(self__281, start__283)
        t224 = inline400
        if t224 {
            var t225 int
            var inline398 int = _goml_runtime_core_string_len(self__281)
            t225 = inline398
            var t226 string
            var inline396 string = string_byte_slice(self__281, start__283, t225)
            t226 = inline396
            var inline394 bool = t226 == suffix__282
            return inline394
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__284 string, expected__285 string) bool {
    var t232 int
    var inline440 int = _goml_runtime_core_string_len(expected__285)
    t232 = inline440
    var t233 bool
    var inline437 int = 0
    var inline438 bool = t232 == inline437
    t233 = inline438
    if t233 {
        return true
    } else {
        var t236 int
        var inline435 int = _goml_runtime_core_string_len(expected__285)
        t236 = inline435
        var t237 int
        var inline433 int = _goml_runtime_core_string_len(self__284)
        t237 = inline433
        var t238 bool = t236 > t237
        if t238 {
            return false
        } else {
            var t239 int
            var inline431 int = _goml_runtime_core_string_len(self__284)
            t239 = inline431
            var t240 int
            var inline429 int = _goml_runtime_core_string_len(expected__285)
            t240 = inline429
            var t241 int = t239 - t240
            var t242 int = t241 + 1
            var t243 FnIterator__int
            var inline424 int = 0
            var inline425 *ref_int_x = ref__Ref_3int(inline424)
            var inline426 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline425,
                end_1: t242,
            }
            var inline427 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline426)
            })
            t243 = inline427
            var for_iter147 FnIterator__int
            for_iter147 = t243
            Loop_loop245:
            for {
                var for_next148 Option__int
                var inline420 func() Option__int = for_iter147.next_fn
                var inline421 Option__int = inline420()
                for_next148 = inline421
                switch for_next148.(type) {
                case None:
                    break Loop_loop245
                case Some:
                    var x149 int = for_next148.(Some)._0
                    var t247 int
                    var inline418 int = _goml_runtime_core_string_len(expected__285)
                    t247 = inline418
                    var end__287 int = x149 + t247
                    var t255 bool
                    var inline416 bool = string_is_char_boundary(self__284, x149)
                    t255 = inline416
                    var jp252 bool
                    if t255 {
                        var inline410 bool = string_is_char_boundary(self__284, end__287)
                        jp252 = inline410
                    } else {
                        jp252 = false
                    }
                    var jp250 bool
                    if jp252 {
                        var t253 string
                        var inline414 string = string_byte_slice(self__284, x149, end__287)
                        t253 = inline414
                        var inline412 bool = t253 == expected__285
                        jp250 = inline412
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

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t259 string = _goml_runtime_core_bool_to_string(self__66)
    return t259
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t262 int = _goml_runtime_core_string_len(self__38)
    return t262
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t265 bool = string_is_char_boundary(self__46, index__47)
    return t265
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline442 bool = string_is_char_boundary(self__43, start__44)
    var inline444 bool
    if inline442 {
        var inline447 bool = string_is_char_boundary(self__43, end__45)
        inline444 = inline447
    } else {
        inline444 = false
    }
    if inline444 {
        var inline445 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline445
    } else {
        var inline446 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline446
    }
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t271 bool = self__99 == other__100
    return t271
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t299 bool = index__16 < 0
    var jp290 bool
    if t299 {
        jp290 = true
    } else {
        var t300 int
        var inline449 int = _goml_runtime_core_string_len(value__15)
        t300 = inline449
        var t301 bool = index__16 > t300
        jp290 = t301
    }
    if jp290 {
        return false
    } else {
        var t293 int
        var inline458 int = _goml_runtime_core_string_len(value__15)
        t293 = inline458
        var t294 bool
        var inline456 bool = index__16 == t293
        t294 = inline456
        if t294 {
            return true
        } else {
            var t295 uint8
            var inline454 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t295 = inline454
            var t296_rhs uint8 = 192
            var t296 uint8 = t295 & t296_rhs
            var t297 bool
            var inline451 uint8 = 128
            var inline452 bool = t296 == inline451
            t297 = inline452
            var t298 bool = !t297
            return t298
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t310 bool = string_is_char_boundary(value__21, start__22)
    var jp307 bool
    if t310 {
        var t311 bool = string_is_char_boundary(value__21, end__23)
        jp307 = t311
    } else {
        jp307 = false
    }
    if jp307 {
        var t308 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t308
    } else {
        var t309 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t309
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__172 func() Option__int) FnIterator__int {
    var t314 FnIterator__int = FnIterator__int{
        next_fn: next_fn__172,
    }
    return t314
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env182 closure_env_goml_builtin_range_0) Option__int {
    var current__270 *ref_int_x = env182.current_0
    var end__269 int = env182.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t328 bool = value__271 < end__269
    if t328 {
        var t329 int = value__271 + 1
        ref_set__Ref_3int(current__270, t329)
        var t330 Option__int = Some{
            _0: value__271,
        }
        return t330
    } else {
        return None{}
    }
}

func main() {
    main0()
}
