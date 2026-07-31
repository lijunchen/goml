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
    var t164 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    println__T_bool(t164)
    var t165 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    println__T_bool(t165)
    var t166 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    println__T_bool(t166)
    var t167 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    println__T_bool(t167)
    var t168 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    println__T_bool(t168)
    var t169 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "")
    println__T_bool(t169)
    var t170 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    println__T_bool(t170)
    var t171 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    println__T_bool(t171)
    var t172 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    println__T_bool(t172)
    var t173 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    println__T_bool(t173)
    var t174 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    println__T_bool(t174)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t177 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t177)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__228 string, prefix__229 string) bool {
    var retv180 bool
    var t188 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
    var t189 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__228)
    var t190 bool = t188 <= t189
    var jp184 bool
    if t190 {
        var t191 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t192 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__228, t191)
        jp184 = t192
    } else {
        jp184 = false
    }
    var jp182 bool
    if jp184 {
        var t185 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t186 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__228, 0, t185)
        var t187 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t186, prefix__229)
        jp182 = t187
    } else {
        jp182 = false
    }
    retv180 = jp182
    return retv180
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__230 string, suffix__231 string) bool {
    var retv194 bool
    var t197 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
    var t198 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
    var t199 bool = t197 > t198
    var jp196 bool
    if t199 {
        jp196 = false
    } else {
        var t200 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
        var t201 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
        var start__232 int = t200 - t201
        var t204 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__230, start__232)
        var jp203 bool
        if t204 {
            var t205 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
            var t206 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__230, start__232, t205)
            var t207 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t206, suffix__231)
            jp203 = t207
        } else {
            jp203 = false
        }
        jp196 = jp203
    }
    retv194 = jp196
    return retv194
}

func _goml_m_inherent_i_string_i_string_i_contains(self__233 string, expected__234 string) bool {
    var retv209 bool
    var t212 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
    var t213 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t212, 0)
    var jp211 bool
    if t213 {
        jp211 = true
        retv209 = jp211
        return retv209
    } else {
        var t216 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
        var t217 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
        var t218 bool = t216 > t217
        var jp215 bool
        if t218 {
            jp215 = false
            jp211 = jp215
            retv209 = jp211
            return retv209
        } else {
            var t219 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
            var t220 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
            var t221 int = t219 - t220
            var t222 int = t221 + 1
            var t223 FnIterator__int = __goml_builtin_range(0, t222)
            var for_iter105 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t223)
            Loop_loop225:
            for {
                if true {
                    var for_next106 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter105)
                    switch for_next106.(type) {
                    case None:
                        break Loop_loop225
                    case Some:
                        var x107 int = for_next106.(Some)._0
                        var start__235 int = x107
                        var t227 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
                        var end__236 int = start__235 + t227
                        var t235 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, start__235)
                        var jp232 bool
                        if t235 {
                            var t236 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, end__236)
                            jp232 = t236
                        } else {
                            jp232 = false
                        }
                        var jp230 bool
                        if jp232 {
                            var t233 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__233, start__235, end__236)
                            var t234 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t233, expected__234)
                            jp230 = t234
                        } else {
                            jp230 = false
                        }
                        if jp230 {
                            retv209 = true
                            return retv209
                        } else {
                            continue
                        }
                    default:
                        panic("non-exhaustive match")
                    }
                } else {
                    break Loop_loop225
                }
            }
            jp215 = false
            jp211 = jp215
            retv209 = jp211
            return retv209
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv238 string
    var t239 string = _goml_runtime_core_bool_to_string(self__37)
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv241 int
    var t242 int = _goml_runtime_core_string_len(self__9)
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv244 bool
    var t245 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv244 = t245
    return retv244
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv247 string
    var t248 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv247 = t248
    return retv247
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv250 bool
    var t251 bool = self__55 == other__56
    retv250 = t251
    return retv250
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv253 bool
    var t254 bool = self__59 == other__60
    retv253 = t254
    return retv253
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv256 FnIterator__int
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t257 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__220,
        end_1: end__219,
    }
    var t258 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t257)
    })
    retv256 = t258
    return retv256
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv260 FnIterator__int
    retv260 = self__109
    return retv260
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv262 Option__int
    var t263 func() Option__int = self__102.next_fn
    var t264 Option__int = t263()
    retv262 = t264
    return retv262
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv266 FnIterator__int
    var t267 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env162 closure_env_goml_builtin_range_0) Option__int {
    var retv272 Option__int
    var current__220 *ref_int_x = env162.current_0
    var end__219 int = env162.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t275 bool = value__221 < end__219
    var jp274 Option__int
    if t275 {
        var t276 int = value__221 + 1
        ref_set__Ref_3int(current__220, t276)
        var t277 Option__int = Some{
            _0: value__221,
        }
        jp274 = t277
    } else {
        jp274 = None{}
    }
    retv272 = jp274
    return retv272
}

func main() {
    main0()
}
