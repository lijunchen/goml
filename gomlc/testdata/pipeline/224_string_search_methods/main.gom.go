package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
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

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
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
    var t167 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline324 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t167)
    _goml_runtime_core_string_println(inline324)
    var t168 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline321 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t168)
    _goml_runtime_core_string_println(inline321)
    var t169 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline318 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t169)
    _goml_runtime_core_string_println(inline318)
    var t170 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline315 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t170)
    _goml_runtime_core_string_println(inline315)
    var t171 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline312 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t171)
    _goml_runtime_core_string_println(inline312)
    var t172 bool
    var inline300 string = ""
    var inline301 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline300)
    var inline302 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline303 bool = inline301 > inline302
    if inline303 {
        t172 = false
    } else {
        var inline304 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline305 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline300)
        var inline306 int = inline304 - inline305
        var inline307 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline306)
        if inline307 {
            var inline308 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline309 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline306, inline308)
            var inline310 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline309, inline300)
            t172 = inline310
        } else {
            t172 = false
        }
    }
    var inline297 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t172)
    _goml_runtime_core_string_println(inline297)
    var t173 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline294 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t173)
    _goml_runtime_core_string_println(inline294)
    var t174 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline291 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t174)
    _goml_runtime_core_string_println(inline291)
    var t175 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline288 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t175)
    _goml_runtime_core_string_println(inline288)
    var t176 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline285 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t176)
    _goml_runtime_core_string_println(inline285)
    var t177 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline282 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t177)
    _goml_runtime_core_string_println(inline282)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__228 string, prefix__229 string) bool {
    var t191 int
    var inline342 int = _goml_runtime_core_string_len(prefix__229)
    t191 = inline342
    var t192 int
    var inline340 int = _goml_runtime_core_string_len(self__228)
    t192 = inline340
    var t193 bool = t191 <= t192
    var jp187 bool
    if t193 {
        var t194 int
        var inline331 int = _goml_runtime_core_string_len(prefix__229)
        t194 = inline331
        var inline329 bool = _goml_runtime_core_string_is_char_boundary(self__228, t194)
        jp187 = inline329
    } else {
        jp187 = false
    }
    if jp187 {
        var t188 int
        var inline338 int = _goml_runtime_core_string_len(prefix__229)
        t188 = inline338
        var t189 string
        var inline335 int = 0
        var inline336 string = _goml_runtime_core_string_byte_slice(self__228, inline335, t188)
        t189 = inline336
        var inline333 bool = t189 == prefix__229
        return inline333
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__230 string, suffix__231 string) bool {
    var t200 int
    var inline358 int = _goml_runtime_core_string_len(suffix__231)
    t200 = inline358
    var t201 int
    var inline356 int = _goml_runtime_core_string_len(self__230)
    t201 = inline356
    var t202 bool = t200 > t201
    if t202 {
        return false
    } else {
        var t203 int
        var inline354 int = _goml_runtime_core_string_len(self__230)
        t203 = inline354
        var t204 int
        var inline352 int = _goml_runtime_core_string_len(suffix__231)
        t204 = inline352
        var start__232 int = t203 - t204
        var t207 bool
        var inline350 bool = _goml_runtime_core_string_is_char_boundary(self__230, start__232)
        t207 = inline350
        if t207 {
            var t208 int
            var inline348 int = _goml_runtime_core_string_len(self__230)
            t208 = inline348
            var t209 string
            var inline346 string = _goml_runtime_core_string_byte_slice(self__230, start__232, t208)
            t209 = inline346
            var inline344 bool = t209 == suffix__231
            return inline344
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__233 string, expected__234 string) bool {
    var t215 int
    var inline390 int = _goml_runtime_core_string_len(expected__234)
    t215 = inline390
    var t216 bool
    var inline387 int = 0
    var inline388 bool = t215 == inline387
    t216 = inline388
    if t216 {
        return true
    } else {
        var t219 int
        var inline385 int = _goml_runtime_core_string_len(expected__234)
        t219 = inline385
        var t220 int
        var inline383 int = _goml_runtime_core_string_len(self__233)
        t220 = inline383
        var t221 bool = t219 > t220
        if t221 {
            return false
        } else {
            var t222 int
            var inline381 int = _goml_runtime_core_string_len(self__233)
            t222 = inline381
            var t223 int
            var inline379 int = _goml_runtime_core_string_len(expected__234)
            t223 = inline379
            var t224 int = t222 - t223
            var t225 int = t224 + 1
            var t226 FnIterator__int
            var inline374 int = 0
            var inline375 *ref_int_x = ref__Ref_3int(inline374)
            var inline376 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline375,
                end_1: t225,
            }
            var inline377 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline376)
            })
            t226 = inline377
            var for_iter105 FnIterator__int
            for_iter105 = t226
            Loop_loop228:
            for {
                var for_next106 Option__int
                var inline370 func() Option__int = for_iter105.next_fn
                var inline371 Option__int = inline370()
                for_next106 = inline371
                switch for_next106.(type) {
                case None:
                    break Loop_loop228
                case Some:
                    var x107 int = for_next106.(Some)._0
                    var t230 int
                    var inline368 int = _goml_runtime_core_string_len(expected__234)
                    t230 = inline368
                    var end__236 int = x107 + t230
                    var t238 bool
                    var inline366 bool = _goml_runtime_core_string_is_char_boundary(self__233, x107)
                    t238 = inline366
                    var jp235 bool
                    if t238 {
                        var inline360 bool = _goml_runtime_core_string_is_char_boundary(self__233, end__236)
                        jp235 = inline360
                    } else {
                        jp235 = false
                    }
                    var jp233 bool
                    if jp235 {
                        var t236 string
                        var inline364 string = _goml_runtime_core_string_byte_slice(self__233, x107, end__236)
                        t236 = inline364
                        var inline362 bool = t236 == expected__234
                        jp233 = inline362
                    } else {
                        jp233 = false
                    }
                    if jp233 {
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

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t242 string = _goml_runtime_core_bool_to_string(self__37)
    return t242
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var t245 int = _goml_runtime_core_string_len(self__9)
    return t245
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var t248 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    return t248
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var t251 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    return t251
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t254 bool = self__55 == other__56
    return t254
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var t270 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    return t270
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env165 closure_env_goml_builtin_range_0) Option__int {
    var current__220 *ref_int_x = env165.current_0
    var end__219 int = env165.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t278 bool = value__221 < end__219
    if t278 {
        var t279 int = value__221 + 1
        ref_set__Ref_3int(current__220, t279)
        var t280 Option__int = Some{
            _0: value__221,
        }
        return t280
    } else {
        return None{}
    }
}

func main() {
    main0()
}
