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
    var inline367 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t184)
    _goml_runtime_core_string_println(inline367)
    var t185 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline364 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline364)
    var t186 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline361 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t186)
    _goml_runtime_core_string_println(inline361)
    var t187 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline358 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t187)
    _goml_runtime_core_string_println(inline358)
    var t188 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline355 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t188)
    _goml_runtime_core_string_println(inline355)
    var t189 bool
    var inline343 string = ""
    var inline344 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline343)
    var inline345 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline346 bool = inline344 > inline345
    if inline346 {
        t189 = false
    } else {
        var inline347 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline348 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline343)
        var inline349 int = inline347 - inline348
        var inline350 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline349)
        if inline350 {
            var inline351 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline352 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline349, inline351)
            var inline353 bool = inline352 == inline343
            t189 = inline353
        } else {
            t189 = false
        }
    }
    var inline340 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    _goml_runtime_core_string_println(inline340)
    var t190 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline337)
    var t191 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline334 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline334)
    var t192 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline331 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    _goml_runtime_core_string_println(inline331)
    var t193 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline328 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    _goml_runtime_core_string_println(inline328)
    var t194 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline325 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t194)
    _goml_runtime_core_string_println(inline325)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__279 string, prefix__280 string) bool {
    var t208 int
    var inline383 int = _goml_runtime_core_string_len(prefix__280)
    t208 = inline383
    var t209 int
    var inline381 int = _goml_runtime_core_string_len(self__279)
    t209 = inline381
    var t210 bool = t208 <= t209
    var jp204 bool
    if t210 {
        var t211 int
        var inline374 int = _goml_runtime_core_string_len(prefix__280)
        t211 = inline374
        var inline372 bool = string_is_char_boundary(self__279, t211)
        jp204 = inline372
    } else {
        jp204 = false
    }
    if jp204 {
        var t205 int
        var inline379 int = _goml_runtime_core_string_len(prefix__280)
        t205 = inline379
        var t206 string
        var inline376 int = 0
        var inline377 string = string_byte_slice(self__279, inline376, t205)
        t206 = inline377
        var t207 bool = t206 == prefix__280
        return t207
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__281 string, suffix__282 string) bool {
    var t217 int
    var inline397 int = _goml_runtime_core_string_len(suffix__282)
    t217 = inline397
    var t218 int
    var inline395 int = _goml_runtime_core_string_len(self__281)
    t218 = inline395
    var t219 bool = t217 > t218
    if t219 {
        return false
    } else {
        var t220 int
        var inline393 int = _goml_runtime_core_string_len(self__281)
        t220 = inline393
        var t221 int
        var inline391 int = _goml_runtime_core_string_len(suffix__282)
        t221 = inline391
        var start__283 int = t220 - t221
        var t224 bool
        var inline389 bool = string_is_char_boundary(self__281, start__283)
        t224 = inline389
        if t224 {
            var t225 int
            var inline387 int = _goml_runtime_core_string_len(self__281)
            t225 = inline387
            var t226 string
            var inline385 string = string_byte_slice(self__281, start__283, t225)
            t226 = inline385
            var t227 bool = t226 == suffix__282
            return t227
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__284 string, expected__285 string) bool {
    var t232 int
    var inline424 int = _goml_runtime_core_string_len(expected__285)
    t232 = inline424
    var t233 bool = t232 == 0
    if t233 {
        return true
    } else {
        var t236 int
        var inline422 int = _goml_runtime_core_string_len(expected__285)
        t236 = inline422
        var t237 int
        var inline420 int = _goml_runtime_core_string_len(self__284)
        t237 = inline420
        var t238 bool = t236 > t237
        if t238 {
            return false
        } else {
            var t239 int
            var inline418 int = _goml_runtime_core_string_len(self__284)
            t239 = inline418
            var t240 int
            var inline416 int = _goml_runtime_core_string_len(expected__285)
            t240 = inline416
            var t241 int = t239 - t240
            var t242 int = t241 + 1
            var t243 FnIterator__int
            var inline411 int = 0
            var inline412 *ref_int_x = ref__Ref_3int(inline411)
            var inline413 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline412,
                end_1: t242,
            }
            var inline414 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline413)
            })
            t243 = inline414
            var for_iter147 FnIterator__int
            for_iter147 = t243
            Loop_loop245:
            for {
                var for_next148 Option__int
                var inline407 func() Option__int = for_iter147.next_fn
                var inline408 Option__int = inline407()
                for_next148 = inline408
                switch for_next148.(type) {
                case None:
                    break Loop_loop245
                case Some:
                    var x149 int = for_next148.(Some)._0
                    var t247 int
                    var inline405 int = _goml_runtime_core_string_len(expected__285)
                    t247 = inline405
                    var end__287 int = x149 + t247
                    var t255 bool
                    var inline403 bool = string_is_char_boundary(self__284, x149)
                    t255 = inline403
                    var jp252 bool
                    if t255 {
                        var inline399 bool = string_is_char_boundary(self__284, end__287)
                        jp252 = inline399
                    } else {
                        jp252 = false
                    }
                    var jp250 bool
                    if jp252 {
                        var t253 string
                        var inline401 string = string_byte_slice(self__284, x149, end__287)
                        t253 = inline401
                        var t254 bool = t253 == expected__285
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
    var inline426 bool = string_is_char_boundary(self__43, start__44)
    var inline428 bool
    if inline426 {
        var inline431 bool = string_is_char_boundary(self__43, end__45)
        inline428 = inline431
    } else {
        inline428 = false
    }
    if inline428 {
        var inline429 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline429
    } else {
        var inline430 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline430
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t292 bool = index__16 < 0
    var jp284 bool
    if t292 {
        jp284 = true
    } else {
        var t293 int
        var inline433 int = _goml_runtime_core_string_len(value__15)
        t293 = inline433
        var t294 bool = index__16 > t293
        jp284 = t294
    }
    if jp284 {
        return false
    } else {
        var t287 int
        var inline437 int = _goml_runtime_core_string_len(value__15)
        t287 = inline437
        var t288 bool = index__16 == t287
        if t288 {
            return true
        } else {
            var t289 uint8
            var inline435 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t289 = inline435
            var t290_rhs uint8 = 192
            var t290 uint8 = t289 & t290_rhs
            var t291 bool = t290 != 128
            return t291
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t303 bool = string_is_char_boundary(value__21, start__22)
    var jp300 bool
    if t303 {
        var t304 bool = string_is_char_boundary(value__21, end__23)
        jp300 = t304
    } else {
        jp300 = false
    }
    if jp300 {
        var t301 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t301
    } else {
        var t302 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t302
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__172 func() Option__int) FnIterator__int {
    var t307 FnIterator__int = FnIterator__int{
        next_fn: next_fn__172,
    }
    return t307
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env182 closure_env_goml_builtin_range_0) Option__int {
    var current__270 *ref_int_x = env182.current_0
    var end__269 int = env182.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t321 bool = value__271 < end__269
    if t321 {
        var t322 int = value__271 + 1
        ref_set__Ref_3int(current__270, t322)
        var t323 Option__int = Some{
            _0: value__271,
        }
        return t323
    } else {
        return None{}
    }
}

func main() {
    main0()
}
