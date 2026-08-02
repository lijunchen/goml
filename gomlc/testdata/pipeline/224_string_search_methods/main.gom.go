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
    println__T_bool(t167)
    var t168 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    println__T_bool(t168)
    var t169 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    println__T_bool(t169)
    var t170 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    println__T_bool(t170)
    var t171 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    println__T_bool(t171)
    var t172 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "")
    println__T_bool(t172)
    var t173 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    println__T_bool(t173)
    var t174 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    println__T_bool(t174)
    var t175 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    println__T_bool(t175)
    var t176 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    println__T_bool(t176)
    var t177 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    println__T_bool(t177)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t180 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__228 string, prefix__229 string) bool {
    var t191 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
    var t192 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__228)
    var t193 bool = t191 <= t192
    var jp187 bool
    if t193 {
        var t194 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t195 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__228, t194)
        jp187 = t195
    } else {
        jp187 = false
    }
    if jp187 {
        var t188 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t189 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__228, 0, t188)
        var t190 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t189, prefix__229)
        return t190
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__230 string, suffix__231 string) bool {
    var t200 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
    var t201 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
    var t202 bool = t200 > t201
    if t202 {
        return false
    } else {
        var t203 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
        var t204 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
        var start__232 int = t203 - t204
        var t207 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__230, start__232)
        if t207 {
            var t208 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
            var t209 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__230, start__232, t208)
            var t210 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t209, suffix__231)
            return t210
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__233 string, expected__234 string) bool {
    var t215 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
    var t216 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t215, 0)
    if t216 {
        return true
    } else {
        var t219 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
        var t220 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
        var t221 bool = t219 > t220
        if t221 {
            return false
        } else {
            var t222 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
            var t223 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
            var t224 int = t222 - t223
            var t225 int = t224 + 1
            var t226 FnIterator__int = __goml_builtin_range(0, t225)
            var for_iter105 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t226)
            Loop_loop228:
            for {
                var for_next106 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter105)
                switch for_next106.(type) {
                case None:
                    break Loop_loop228
                case Some:
                    var x107 int = for_next106.(Some)._0
                    var t230 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
                    var end__236 int = x107 + t230
                    var t238 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, x107)
                    var jp235 bool
                    if t238 {
                        var t239 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, end__236)
                        jp235 = t239
                    } else {
                        jp235 = false
                    }
                    var jp233 bool
                    if jp235 {
                        var t236 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__233, x107, end__236)
                        var t237 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t236, expected__234)
                        jp233 = t237
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

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t257 bool = self__59 == other__60
    return t257
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t260 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__220,
        end_1: end__219,
    }
    var t261 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t260)
    })
    return t261
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    return self__109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var t266 func() Option__int = self__102.next_fn
    var t267 Option__int = t266()
    return t267
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
