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
    var t148 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline338 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t148)
    _goml_runtime_core_string_println(inline338)
    var t149 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline335 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t149)
    _goml_runtime_core_string_println(inline335)
    var t150 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline332 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t150)
    _goml_runtime_core_string_println(inline332)
    var t151 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline329 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t151)
    _goml_runtime_core_string_println(inline329)
    var t152 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline326 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t152)
    _goml_runtime_core_string_println(inline326)
    var t153 bool
    var inline314 string = ""
    var inline315 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline314)
    var inline316 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline317 bool = inline315 > inline316
    if inline317 {
        t153 = false
    } else {
        var inline318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline319 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline314)
        var inline320 int = inline318 - inline319
        var inline321 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline320)
        if inline321 {
            var inline322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline323 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline320, inline322)
            var inline324 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline323, inline314)
            t153 = inline324
        } else {
            t153 = false
        }
    }
    var inline311 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t153)
    _goml_runtime_core_string_println(inline311)
    var t154 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline308 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t154)
    _goml_runtime_core_string_println(inline308)
    var t155 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline305 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t155)
    _goml_runtime_core_string_println(inline305)
    var t156 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline302 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t156)
    _goml_runtime_core_string_println(inline302)
    var t157 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline299 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t157)
    _goml_runtime_core_string_println(inline299)
    var t158 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline296 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t158)
    _goml_runtime_core_string_println(inline296)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__254 string, prefix__255 string) bool {
    var t172 int
    var inline356 int = _goml_runtime_core_string_len(prefix__255)
    t172 = inline356
    var t173 int
    var inline354 int = _goml_runtime_core_string_len(self__254)
    t173 = inline354
    var t174 bool = t172 <= t173
    var jp168 bool
    if t174 {
        var t175 int
        var inline345 int = _goml_runtime_core_string_len(prefix__255)
        t175 = inline345
        var inline343 bool = string_is_char_boundary(self__254, t175)
        jp168 = inline343
    } else {
        jp168 = false
    }
    if jp168 {
        var t169 int
        var inline352 int = _goml_runtime_core_string_len(prefix__255)
        t169 = inline352
        var t170 string
        var inline349 int = 0
        var inline350 string = string_byte_slice(self__254, inline349, t169)
        t170 = inline350
        var inline347 bool = t170 == prefix__255
        return inline347
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__256 string, suffix__257 string) bool {
    var t181 int
    var inline372 int = _goml_runtime_core_string_len(suffix__257)
    t181 = inline372
    var t182 int
    var inline370 int = _goml_runtime_core_string_len(self__256)
    t182 = inline370
    var t183 bool = t181 > t182
    if t183 {
        return false
    } else {
        var t184 int
        var inline368 int = _goml_runtime_core_string_len(self__256)
        t184 = inline368
        var t185 int
        var inline366 int = _goml_runtime_core_string_len(suffix__257)
        t185 = inline366
        var start__258 int = t184 - t185
        var t188 bool
        var inline364 bool = string_is_char_boundary(self__256, start__258)
        t188 = inline364
        if t188 {
            var t189 int
            var inline362 int = _goml_runtime_core_string_len(self__256)
            t189 = inline362
            var t190 string
            var inline360 string = string_byte_slice(self__256, start__258, t189)
            t190 = inline360
            var inline358 bool = t190 == suffix__257
            return inline358
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__259 string, expected__260 string) bool {
    var t196 int
    var inline404 int = _goml_runtime_core_string_len(expected__260)
    t196 = inline404
    var t197 bool
    var inline401 int = 0
    var inline402 bool = t196 == inline401
    t197 = inline402
    if t197 {
        return true
    } else {
        var t200 int
        var inline399 int = _goml_runtime_core_string_len(expected__260)
        t200 = inline399
        var t201 int
        var inline397 int = _goml_runtime_core_string_len(self__259)
        t201 = inline397
        var t202 bool = t200 > t201
        if t202 {
            return false
        } else {
            var t203 int
            var inline395 int = _goml_runtime_core_string_len(self__259)
            t203 = inline395
            var t204 int
            var inline393 int = _goml_runtime_core_string_len(expected__260)
            t204 = inline393
            var t205 int = t203 - t204
            var t206 int = t205 + 1
            var t207 FnIterator__int
            var inline388 int = 0
            var inline389 *ref_int_x = ref__Ref_3int(inline388)
            var inline390 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline389,
                end_1: t206,
            }
            var inline391 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline390)
            })
            t207 = inline391
            var for_iter111 FnIterator__int
            for_iter111 = t207
            Loop_loop209:
            for {
                var for_next112 Option__int
                var inline384 func() Option__int = for_iter111.next_fn
                var inline385 Option__int = inline384()
                for_next112 = inline385
                switch for_next112.(type) {
                case None:
                    break Loop_loop209
                case Some:
                    var x113 int = for_next112.(Some)._0
                    var t211 int
                    var inline382 int = _goml_runtime_core_string_len(expected__260)
                    t211 = inline382
                    var end__262 int = x113 + t211
                    var t219 bool
                    var inline380 bool = string_is_char_boundary(self__259, x113)
                    t219 = inline380
                    var jp216 bool
                    if t219 {
                        var inline374 bool = string_is_char_boundary(self__259, end__262)
                        jp216 = inline374
                    } else {
                        jp216 = false
                    }
                    var jp214 bool
                    if jp216 {
                        var t217 string
                        var inline378 string = string_byte_slice(self__259, x113, end__262)
                        t217 = inline378
                        var inline376 bool = t217 == expected__260
                        jp214 = inline376
                    } else {
                        jp214 = false
                    }
                    if jp214 {
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
    var t223 string = _goml_runtime_core_bool_to_string(self__66)
    return t223
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t226 int = _goml_runtime_core_string_len(self__38)
    return t226
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t229 bool = string_is_char_boundary(self__46, index__47)
    return t229
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline406 bool = string_is_char_boundary(self__43, start__44)
    var inline408 bool
    if inline406 {
        var inline411 bool = string_is_char_boundary(self__43, end__45)
        inline408 = inline411
    } else {
        inline408 = false
    }
    if inline408 {
        var inline409 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline409
    } else {
        var inline410 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline410
    }
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t235 bool = self__99 == other__100
    return t235
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t263 bool = index__16 < 0
    var jp254 bool
    if t263 {
        jp254 = true
    } else {
        var t264 int
        var inline413 int = _goml_runtime_core_string_len(value__15)
        t264 = inline413
        var t265 bool = index__16 > t264
        jp254 = t265
    }
    if jp254 {
        return false
    } else {
        var t257 int
        var inline422 int = _goml_runtime_core_string_len(value__15)
        t257 = inline422
        var t258 bool
        var inline420 bool = index__16 == t257
        t258 = inline420
        if t258 {
            return true
        } else {
            var t259 uint8
            var inline418 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t259 = inline418
            var t260_rhs uint8 = 192
            var t260 uint8 = t259 & t260_rhs
            var t261 bool
            var inline415 uint8 = 128
            var inline416 bool = t260 == inline415
            t261 = inline416
            var t262 bool = !t261
            return t262
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t274 bool = string_is_char_boundary(value__21, start__22)
    var jp271 bool
    if t274 {
        var t275 bool = string_is_char_boundary(value__21, end__23)
        jp271 = t275
    } else {
        jp271 = false
    }
    if jp271 {
        var t272 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t272
    } else {
        var t273 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t273
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__147 func() Option__int) FnIterator__int {
    var t278 FnIterator__int = FnIterator__int{
        next_fn: next_fn__147,
    }
    return t278
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env146 closure_env_goml_builtin_range_0) Option__int {
    var current__245 *ref_int_x = env146.current_0
    var end__244 int = env146.end_1
    var value__246 int = ref_get__Ref_3int(current__245)
    var t292 bool = value__246 < end__244
    if t292 {
        var t293 int = value__246 + 1
        ref_set__Ref_3int(current__245, t293)
        var t294 Option__int = Some{
            _0: value__246,
        }
        return t294
    } else {
        return None{}
    }
}

func main() {
    main0()
}
