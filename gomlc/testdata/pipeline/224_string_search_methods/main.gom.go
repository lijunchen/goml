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
    var t189 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline379 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    _goml_runtime_core_string_println(inline379)
    var t190 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline376 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline376)
    var t191 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline373 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline373)
    var t192 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline370 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    _goml_runtime_core_string_println(inline370)
    var t193 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline367 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    _goml_runtime_core_string_println(inline367)
    var t194 bool
    var inline355 string = ""
    var inline356 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline355)
    var inline357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline358 bool = inline356 > inline357
    if inline358 {
        t194 = false
    } else {
        var inline359 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline360 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline355)
        var inline361 int = inline359 - inline360
        var inline362 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline361)
        if inline362 {
            var inline363 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline364 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline361, inline363)
            var inline365 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline364, inline355)
            t194 = inline365
        } else {
            t194 = false
        }
    }
    var inline352 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t194)
    _goml_runtime_core_string_println(inline352)
    var t195 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline349 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    _goml_runtime_core_string_println(inline349)
    var t196 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline346 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t196)
    _goml_runtime_core_string_println(inline346)
    var t197 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline343 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t197)
    _goml_runtime_core_string_println(inline343)
    var t198 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline340 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    _goml_runtime_core_string_println(inline340)
    var t199 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    _goml_runtime_core_string_println(inline337)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__257 string, prefix__258 string) bool {
    var t213 int
    var inline397 int = _goml_runtime_core_string_len(prefix__258)
    t213 = inline397
    var t214 int
    var inline395 int = _goml_runtime_core_string_len(self__257)
    t214 = inline395
    var t215 bool = t213 <= t214
    var jp209 bool
    if t215 {
        var t216 int
        var inline386 int = _goml_runtime_core_string_len(prefix__258)
        t216 = inline386
        var inline384 bool = string_is_char_boundary(self__257, t216)
        jp209 = inline384
    } else {
        jp209 = false
    }
    if jp209 {
        var t210 int
        var inline393 int = _goml_runtime_core_string_len(prefix__258)
        t210 = inline393
        var t211 string
        var inline390 int = 0
        var inline391 string = string_byte_slice(self__257, inline390, t210)
        t211 = inline391
        var inline388 bool = t211 == prefix__258
        return inline388
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__259 string, suffix__260 string) bool {
    var t222 int
    var inline413 int = _goml_runtime_core_string_len(suffix__260)
    t222 = inline413
    var t223 int
    var inline411 int = _goml_runtime_core_string_len(self__259)
    t223 = inline411
    var t224 bool = t222 > t223
    if t224 {
        return false
    } else {
        var t225 int
        var inline409 int = _goml_runtime_core_string_len(self__259)
        t225 = inline409
        var t226 int
        var inline407 int = _goml_runtime_core_string_len(suffix__260)
        t226 = inline407
        var start__261 int = t225 - t226
        var t229 bool
        var inline405 bool = string_is_char_boundary(self__259, start__261)
        t229 = inline405
        if t229 {
            var t230 int
            var inline403 int = _goml_runtime_core_string_len(self__259)
            t230 = inline403
            var t231 string
            var inline401 string = string_byte_slice(self__259, start__261, t230)
            t231 = inline401
            var inline399 bool = t231 == suffix__260
            return inline399
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__262 string, expected__263 string) bool {
    var t237 int
    var inline445 int = _goml_runtime_core_string_len(expected__263)
    t237 = inline445
    var t238 bool
    var inline442 int = 0
    var inline443 bool = t237 == inline442
    t238 = inline443
    if t238 {
        return true
    } else {
        var t241 int
        var inline440 int = _goml_runtime_core_string_len(expected__263)
        t241 = inline440
        var t242 int
        var inline438 int = _goml_runtime_core_string_len(self__262)
        t242 = inline438
        var t243 bool = t241 > t242
        if t243 {
            return false
        } else {
            var t244 int
            var inline436 int = _goml_runtime_core_string_len(self__262)
            t244 = inline436
            var t245 int
            var inline434 int = _goml_runtime_core_string_len(expected__263)
            t245 = inline434
            var t246 int = t244 - t245
            var t247 int = t246 + 1
            var t248 FnIterator__int
            var inline429 int = 0
            var inline430 *ref_int_x = ref__Ref_3int(inline429)
            var inline431 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline430,
                end_1: t247,
            }
            var inline432 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline431)
            })
            t248 = inline432
            var for_iter127 FnIterator__int
            for_iter127 = t248
            Loop_loop250:
            for {
                var for_next128 Option__int
                var inline425 func() Option__int = for_iter127.next_fn
                var inline426 Option__int = inline425()
                for_next128 = inline426
                switch for_next128.(type) {
                case None:
                    break Loop_loop250
                case Some:
                    var x129 int = for_next128.(Some)._0
                    var t252 int
                    var inline423 int = _goml_runtime_core_string_len(expected__263)
                    t252 = inline423
                    var end__265 int = x129 + t252
                    var t260 bool
                    var inline421 bool = string_is_char_boundary(self__262, x129)
                    t260 = inline421
                    var jp257 bool
                    if t260 {
                        var inline415 bool = string_is_char_boundary(self__262, end__265)
                        jp257 = inline415
                    } else {
                        jp257 = false
                    }
                    var jp255 bool
                    if jp257 {
                        var t258 string
                        var inline419 string = string_byte_slice(self__262, x129, end__265)
                        t258 = inline419
                        var inline417 bool = t258 == expected__263
                        jp255 = inline417
                    } else {
                        jp255 = false
                    }
                    if jp255 {
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
    var t264 string = _goml_runtime_core_bool_to_string(self__66)
    return t264
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t267 int = _goml_runtime_core_string_len(self__38)
    return t267
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t270 bool = string_is_char_boundary(self__46, index__47)
    return t270
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline447 bool = string_is_char_boundary(self__43, start__44)
    var inline449 bool
    if inline447 {
        var inline452 bool = string_is_char_boundary(self__43, end__45)
        inline449 = inline452
    } else {
        inline449 = false
    }
    if inline449 {
        var inline450 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline450
    } else {
        var inline451 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline451
    }
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t276 bool = self__84 == other__85
    return t276
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t304 bool = index__16 < 0
    var jp295 bool
    if t304 {
        jp295 = true
    } else {
        var t305 int
        var inline454 int = _goml_runtime_core_string_len(value__15)
        t305 = inline454
        var t306 bool = index__16 > t305
        jp295 = t306
    }
    if jp295 {
        return false
    } else {
        var t298 int
        var inline463 int = _goml_runtime_core_string_len(value__15)
        t298 = inline463
        var t299 bool
        var inline461 bool = index__16 == t298
        t299 = inline461
        if t299 {
            return true
        } else {
            var t300 uint8
            var inline459 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t300 = inline459
            var t301_rhs uint8 = 192
            var t301 uint8 = t300 & t301_rhs
            var t302 bool
            var inline456 uint8 = 128
            var inline457 bool = t301 == inline456
            t302 = inline457
            var t303 bool = !t302
            return t303
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t315 bool = string_is_char_boundary(value__21, start__22)
    var jp312 bool
    if t315 {
        var t316 bool = string_is_char_boundary(value__21, end__23)
        jp312 = t316
    } else {
        jp312 = false
    }
    if jp312 {
        var t313 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t313
    } else {
        var t314 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t314
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t319 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t319
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env187 closure_env_goml_builtin_range_0) Option__int {
    var current__249 *ref_int_x = env187.current_0
    var end__248 int = env187.end_1
    var value__250 int = ref_get__Ref_3int(current__249)
    var t333 bool = value__250 < end__248
    if t333 {
        var t334 int = value__250 + 1
        ref_set__Ref_3int(current__249, t334)
        var t335 Option__int = Some{
            _0: value__250,
        }
        return t335
    } else {
        return None{}
    }
}

func main() {
    main0()
}
