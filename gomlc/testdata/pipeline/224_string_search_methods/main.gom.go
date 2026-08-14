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
    var t194 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline378 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t194)
    _goml_runtime_core_string_println(inline378)
    var t195 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline375 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    _goml_runtime_core_string_println(inline375)
    var t196 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline372 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t196)
    _goml_runtime_core_string_println(inline372)
    var t197 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline369 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t197)
    _goml_runtime_core_string_println(inline369)
    var t198 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline366 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    _goml_runtime_core_string_println(inline366)
    var t199 bool
    var inline354 string = ""
    var inline355 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline354)
    var inline356 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline357 bool = inline355 > inline356
    if inline357 {
        t199 = false
    } else {
        var inline358 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline359 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline354)
        var inline360 int = inline358 - inline359
        var inline361 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline360)
        if inline361 {
            var inline362 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline363 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline360, inline362)
            var inline364 bool = inline363 == inline354
            t199 = inline364
        } else {
            t199 = false
        }
    }
    var inline351 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    _goml_runtime_core_string_println(inline351)
    var t200 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline348 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t200)
    _goml_runtime_core_string_println(inline348)
    var t201 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline345 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    _goml_runtime_core_string_println(inline345)
    var t202 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline342 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t202)
    _goml_runtime_core_string_println(inline342)
    var t203 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline339 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t203)
    _goml_runtime_core_string_println(inline339)
    var t204 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline336 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t204)
    _goml_runtime_core_string_println(inline336)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__286 string, prefix__287 string) bool {
    var t218 int
    var inline394 int = _goml_runtime_core_string_len(prefix__287)
    t218 = inline394
    var t219 int
    var inline392 int = _goml_runtime_core_string_len(self__286)
    t219 = inline392
    var t220 bool = t218 <= t219
    var jp214 bool
    if t220 {
        var t221 int
        var inline385 int = _goml_runtime_core_string_len(prefix__287)
        t221 = inline385
        var inline383 bool = string_is_char_boundary(self__286, t221)
        jp214 = inline383
    } else {
        jp214 = false
    }
    if jp214 {
        var t215 int
        var inline390 int = _goml_runtime_core_string_len(prefix__287)
        t215 = inline390
        var t216 string
        var inline387 int = 0
        var inline388 string = string_byte_slice(self__286, inline387, t215)
        t216 = inline388
        var t217 bool = t216 == prefix__287
        return t217
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__288 string, suffix__289 string) bool {
    var t227 int
    var inline408 int = _goml_runtime_core_string_len(suffix__289)
    t227 = inline408
    var t228 int
    var inline406 int = _goml_runtime_core_string_len(self__288)
    t228 = inline406
    var t229 bool = t227 > t228
    if t229 {
        return false
    } else {
        var t230 int
        var inline404 int = _goml_runtime_core_string_len(self__288)
        t230 = inline404
        var t231 int
        var inline402 int = _goml_runtime_core_string_len(suffix__289)
        t231 = inline402
        var start__290 int = t230 - t231
        var t234 bool
        var inline400 bool = string_is_char_boundary(self__288, start__290)
        t234 = inline400
        if t234 {
            var t235 int
            var inline398 int = _goml_runtime_core_string_len(self__288)
            t235 = inline398
            var t236 string
            var inline396 string = string_byte_slice(self__288, start__290, t235)
            t236 = inline396
            var t237 bool = t236 == suffix__289
            return t237
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__291 string, expected__292 string) bool {
    var t242 int
    var inline436 int = _goml_runtime_core_string_len(expected__292)
    t242 = inline436
    var t243 bool = t242 == 0
    if t243 {
        return true
    } else {
        var t246 int
        var inline434 int = _goml_runtime_core_string_len(expected__292)
        t246 = inline434
        var t247 int
        var inline432 int = _goml_runtime_core_string_len(self__291)
        t247 = inline432
        var t248 bool = t246 > t247
        if t248 {
            return false
        } else {
            var t249 int
            var inline430 int = _goml_runtime_core_string_len(self__291)
            t249 = inline430
            var t250 int
            var inline428 int = _goml_runtime_core_string_len(expected__292)
            t250 = inline428
            var t251 int = t249 - t250
            var t252 int = t251 + 1
            var t253 FnIterator__int
            var inline422 int = 0
            var inline423 *ref_int_x = ref__Ref_3int(inline422)
            var inline424 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline423,
                end_1: t252,
            }
            var inline425 func() Option__int = func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline424)
            }
            var inline426 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline425)
            t253 = inline426
            var for_iter156 FnIterator__int
            for_iter156 = t253
            Loop_loop255:
            for {
                var for_next157 Option__int
                var inline418 func() Option__int = for_iter156.next_fn
                var inline419 Option__int = inline418()
                for_next157 = inline419
                switch for_next157.(type) {
                case None:
                    break Loop_loop255
                case Some:
                    var x158 int = for_next157.(Some)._0
                    var t257 int
                    var inline416 int = _goml_runtime_core_string_len(expected__292)
                    t257 = inline416
                    var end__294 int = x158 + t257
                    var t265 bool
                    var inline414 bool = string_is_char_boundary(self__291, x158)
                    t265 = inline414
                    var jp262 bool
                    if t265 {
                        var inline410 bool = string_is_char_boundary(self__291, end__294)
                        jp262 = inline410
                    } else {
                        jp262 = false
                    }
                    var jp260 bool
                    if jp262 {
                        var t263 string
                        var inline412 string = string_byte_slice(self__291, x158, end__294)
                        t263 = inline412
                        var t264 bool = t263 == expected__292
                        jp260 = t264
                    } else {
                        jp260 = false
                    }
                    if jp260 {
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
    var t269 string = _goml_runtime_core_bool_to_string(self__64)
    return t269
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t272 int = _goml_runtime_core_string_len(self__36)
    return t272
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t275 bool = string_is_char_boundary(self__44, index__45)
    return t275
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline438 bool = string_is_char_boundary(self__41, start__42)
    var inline440 bool
    if inline438 {
        var inline443 bool = string_is_char_boundary(self__41, end__43)
        inline440 = inline443
    } else {
        inline440 = false
    }
    if inline440 {
        var inline441 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline441
    } else {
        var inline442 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline442
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t303 bool = index__16 < 0
    var jp295 bool
    if t303 {
        jp295 = true
    } else {
        var t304 int
        var inline447 int = _goml_runtime_core_string_len(value__15)
        t304 = inline447
        var t305 bool = index__16 > t304
        jp295 = t305
    }
    if jp295 {
        return false
    } else {
        var t298 int
        var inline451 int = _goml_runtime_core_string_len(value__15)
        t298 = inline451
        var t299 bool = index__16 == t298
        if t299 {
            return true
        } else {
            var t300 uint8
            var inline449 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t300 = inline449
            var t301_rhs uint8 = 192
            var t301 uint8 = t300 & t301_rhs
            var t302 bool = t301 != 128
            return t302
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t314 bool = string_is_char_boundary(value__21, start__22)
    var jp311 bool
    if t314 {
        var t315 bool = string_is_char_boundary(value__21, end__23)
        jp311 = t315
    } else {
        jp311 = false
    }
    if jp311 {
        var t312 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t312
    } else {
        var t313 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t313
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__170 func() Option__int) FnIterator__int {
    var t318 FnIterator__int = FnIterator__int{
        next_fn: next_fn__170,
    }
    return t318
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env192 closure_env_goml_builtin_range_0) Option__int {
    var current__335 *ref_int_x = env192.current_0
    var end__334 int = env192.end_1
    var value__336 int = ref_get__Ref_3int(current__335)
    var t332 bool = value__336 < end__334
    if t332 {
        var t333 int = value__336 + 1
        ref_set__Ref_3int(current__335, t333)
        var t334 Option__int = Some{
            _0: value__336,
        }
        return t334
    } else {
        return None{}
    }
}

func main() {
    main0()
}
