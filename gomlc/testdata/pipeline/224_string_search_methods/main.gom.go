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
    var t199 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline383 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    _goml_runtime_core_string_println(inline383)
    var t200 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline380 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t200)
    _goml_runtime_core_string_println(inline380)
    var t201 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline377 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    _goml_runtime_core_string_println(inline377)
    var t202 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline374 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t202)
    _goml_runtime_core_string_println(inline374)
    var t203 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline371 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t203)
    _goml_runtime_core_string_println(inline371)
    var t204 bool
    var inline359 string = ""
    var inline360 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline359)
    var inline361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline362 bool = inline360 > inline361
    if inline362 {
        t204 = false
    } else {
        var inline363 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline364 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline359)
        var inline365 int = inline363 - inline364
        var inline366 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline365)
        if inline366 {
            var inline367 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline368 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline365, inline367)
            var inline369 bool = inline368 == inline359
            t204 = inline369
        } else {
            t204 = false
        }
    }
    var inline356 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t204)
    _goml_runtime_core_string_println(inline356)
    var t205 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline353 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t205)
    _goml_runtime_core_string_println(inline353)
    var t206 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline350 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t206)
    _goml_runtime_core_string_println(inline350)
    var t207 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline347 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t207)
    _goml_runtime_core_string_println(inline347)
    var t208 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline344 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t208)
    _goml_runtime_core_string_println(inline344)
    var t209 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t209)
    _goml_runtime_core_string_println(inline341)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__289 string, prefix__290 string) bool {
    var t223 int
    var inline399 int = _goml_runtime_core_string_len(prefix__290)
    t223 = inline399
    var t224 int
    var inline397 int = _goml_runtime_core_string_len(self__289)
    t224 = inline397
    var t225 bool = t223 <= t224
    var jp219 bool
    if t225 {
        var t226 int
        var inline390 int = _goml_runtime_core_string_len(prefix__290)
        t226 = inline390
        var inline388 bool = string_is_char_boundary(self__289, t226)
        jp219 = inline388
    } else {
        jp219 = false
    }
    if jp219 {
        var t220 int
        var inline395 int = _goml_runtime_core_string_len(prefix__290)
        t220 = inline395
        var t221 string
        var inline392 int = 0
        var inline393 string = string_byte_slice(self__289, inline392, t220)
        t221 = inline393
        var t222 bool = t221 == prefix__290
        return t222
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__291 string, suffix__292 string) bool {
    var t232 int
    var inline413 int = _goml_runtime_core_string_len(suffix__292)
    t232 = inline413
    var t233 int
    var inline411 int = _goml_runtime_core_string_len(self__291)
    t233 = inline411
    var t234 bool = t232 > t233
    if t234 {
        return false
    } else {
        var t235 int
        var inline409 int = _goml_runtime_core_string_len(self__291)
        t235 = inline409
        var t236 int
        var inline407 int = _goml_runtime_core_string_len(suffix__292)
        t236 = inline407
        var start__293 int = t235 - t236
        var t239 bool
        var inline405 bool = string_is_char_boundary(self__291, start__293)
        t239 = inline405
        if t239 {
            var t240 int
            var inline403 int = _goml_runtime_core_string_len(self__291)
            t240 = inline403
            var t241 string
            var inline401 string = string_byte_slice(self__291, start__293, t240)
            t241 = inline401
            var t242 bool = t241 == suffix__292
            return t242
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__294 string, expected__295 string) bool {
    var t247 int
    var inline441 int = _goml_runtime_core_string_len(expected__295)
    t247 = inline441
    var t248 bool = t247 == 0
    if t248 {
        return true
    } else {
        var t251 int
        var inline439 int = _goml_runtime_core_string_len(expected__295)
        t251 = inline439
        var t252 int
        var inline437 int = _goml_runtime_core_string_len(self__294)
        t252 = inline437
        var t253 bool = t251 > t252
        if t253 {
            return false
        } else {
            var t254 int
            var inline435 int = _goml_runtime_core_string_len(self__294)
            t254 = inline435
            var t255 int
            var inline433 int = _goml_runtime_core_string_len(expected__295)
            t255 = inline433
            var t256 int = t254 - t255
            var t257 int = t256 + 1
            var t258 FnIterator__int
            var inline427 int = 0
            var inline428 *ref_int_x = ref__Ref_3int(inline427)
            var inline429 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline428,
                end_1: t257,
            }
            var inline430 func() Option__int = func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline429)
            }
            var inline431 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline430)
            t258 = inline431
            var for_iter161 FnIterator__int
            for_iter161 = t258
            Loop_loop260:
            for {
                var for_next162 Option__int
                var inline423 func() Option__int = for_iter161.next_fn
                var inline424 Option__int = inline423()
                for_next162 = inline424
                switch for_next162.(type) {
                case None:
                    break Loop_loop260
                case Some:
                    var x163 int = for_next162.(Some)._0
                    var t262 int
                    var inline421 int = _goml_runtime_core_string_len(expected__295)
                    t262 = inline421
                    var end__297 int = x163 + t262
                    var t270 bool
                    var inline419 bool = string_is_char_boundary(self__294, x163)
                    t270 = inline419
                    var jp267 bool
                    if t270 {
                        var inline415 bool = string_is_char_boundary(self__294, end__297)
                        jp267 = inline415
                    } else {
                        jp267 = false
                    }
                    var jp265 bool
                    if jp267 {
                        var t268 string
                        var inline417 string = string_byte_slice(self__294, x163, end__297)
                        t268 = inline417
                        var t269 bool = t268 == expected__295
                        jp265 = t269
                    } else {
                        jp265 = false
                    }
                    if jp265 {
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
    var t274 string = _goml_runtime_core_bool_to_string(self__64)
    return t274
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t277 int = _goml_runtime_core_string_len(self__36)
    return t277
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t280 bool = string_is_char_boundary(self__44, index__45)
    return t280
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline443 bool = string_is_char_boundary(self__41, start__42)
    var inline445 bool
    if inline443 {
        var inline448 bool = string_is_char_boundary(self__41, end__43)
        inline445 = inline448
    } else {
        inline445 = false
    }
    if inline445 {
        var inline446 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline446
    } else {
        var inline447 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline447
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t308 bool = index__16 < 0
    var jp300 bool
    if t308 {
        jp300 = true
    } else {
        var t309 int
        var inline452 int = _goml_runtime_core_string_len(value__15)
        t309 = inline452
        var t310 bool = index__16 > t309
        jp300 = t310
    }
    if jp300 {
        return false
    } else {
        var t303 int
        var inline456 int = _goml_runtime_core_string_len(value__15)
        t303 = inline456
        var t304 bool = index__16 == t303
        if t304 {
            return true
        } else {
            var t305 uint8
            var inline454 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t305 = inline454
            var t306_rhs uint8 = 192
            var t306 uint8 = t305 & t306_rhs
            var t307 bool = t306 != 128
            return t307
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t319 bool = string_is_char_boundary(value__21, start__22)
    var jp316 bool
    if t319 {
        var t320 bool = string_is_char_boundary(value__21, end__23)
        jp316 = t320
    } else {
        jp316 = false
    }
    if jp316 {
        var t317 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t317
    } else {
        var t318 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t318
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__170 func() Option__int) FnIterator__int {
    var t323 FnIterator__int = FnIterator__int{
        next_fn: next_fn__170,
    }
    return t323
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env197 closure_env_goml_builtin_range_0) Option__int {
    var current__338 *ref_int_x = env197.current_0
    var end__337 int = env197.end_1
    var value__339 int = ref_get__Ref_3int(current__338)
    var t337 bool = value__339 < end__337
    if t337 {
        var t338 int = value__339 + 1
        ref_set__Ref_3int(current__338, t338)
        var t339 Option__int = Some{
            _0: value__339,
        }
        return t339
    } else {
        return None{}
    }
}

func main() {
    main0()
}
