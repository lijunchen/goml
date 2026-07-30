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
    var t120 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    println__T_bool(t120)
    var t121 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    println__T_bool(t121)
    var t122 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    println__T_bool(t122)
    var t123 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    println__T_bool(t123)
    var t124 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    println__T_bool(t124)
    var t125 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "")
    println__T_bool(t125)
    var t126 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    println__T_bool(t126)
    var t127 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    println__T_bool(t127)
    var t128 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    println__T_bool(t128)
    var t129 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    println__T_bool(t129)
    var t130 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    println__T_bool(t130)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t133 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t133)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__228 string, prefix__229 string) bool {
    var retv136 bool
    var t144 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
    var t145 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__228)
    var t146 bool = t144 <= t145
    var jp140 bool
    if t146 {
        var t147 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t148 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__228, t147)
        jp140 = t148
    } else {
        jp140 = false
    }
    var jp138 bool
    if jp140 {
        var t141 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t142 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__228, 0, t141)
        var t143 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t142, prefix__229)
        jp138 = t143
    } else {
        jp138 = false
    }
    retv136 = jp138
    return retv136
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__230 string, suffix__231 string) bool {
    var retv150 bool
    var t153 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
    var t154 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
    var t155 bool = t153 > t154
    var jp152 bool
    if t155 {
        jp152 = false
    } else {
        var t156 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
        var t157 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
        var start__232 int = t156 - t157
        var t160 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__230, start__232)
        var jp159 bool
        if t160 {
            var t161 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
            var t162 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__230, start__232, t161)
            var t163 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t162, suffix__231)
            jp159 = t163
        } else {
            jp159 = false
        }
        jp152 = jp159
    }
    retv150 = jp152
    return retv150
}

func _goml_m_inherent_i_string_i_string_i_contains(self__233 string, expected__234 string) bool {
    var retv165 bool
    var t168 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
    var t169 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t168, 0)
    var jp167 bool
    if t169 {
        jp167 = true
        retv165 = jp167
        return retv165
    } else {
        var t172 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
        var t173 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
        var t174 bool = t172 > t173
        var jp171 bool
        if t174 {
            jp171 = false
            jp167 = jp171
            retv165 = jp167
            return retv165
        } else {
            var t175 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
            var t176 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
            var t177 int = t175 - t176
            var t178 int = t177 + 1
            var t179 FnIterator__int = _goml_m_range(0, t178)
            var for_iter64 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t179)
            Loop_loop181:
            for {
                if true {
                    var for_next65 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter64)
                    switch for_next65.(type) {
                    case None:
                        break Loop_loop181
                    case Some:
                        var x66 int = for_next65.(Some)._0
                        var start__235 int = x66
                        var t183 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
                        var end__236 int = start__235 + t183
                        var t191 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, start__235)
                        var jp188 bool
                        if t191 {
                            var t192 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, end__236)
                            jp188 = t192
                        } else {
                            jp188 = false
                        }
                        var jp186 bool
                        if jp188 {
                            var t189 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__233, start__235, end__236)
                            var t190 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t189, expected__234)
                            jp186 = t190
                        } else {
                            jp186 = false
                        }
                        if jp186 {
                            retv165 = true
                            return retv165
                        } else {
                            continue
                        }
                    default:
                        panic("non-exhaustive match")
                    }
                } else {
                    break Loop_loop181
                }
            }
            jp171 = false
            jp167 = jp171
            retv165 = jp167
            return retv165
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv194 string
    var t195 string = _goml_runtime_core_bool_to_string(self__37)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv197 int
    var t198 int = _goml_runtime_core_string_len(self__9)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv200 bool
    var t201 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv203 string
    var t204 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv203 = t204
    return retv203
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv206 bool
    var t207 bool = self__55 == other__56
    retv206 = t207
    return retv206
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv209 bool
    var t210 bool = self__59 == other__60
    retv209 = t210
    return retv209
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv212 FnIterator__int
    var t213 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv212 = t213
    return retv212
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv215 FnIterator__int
    retv215 = self__109
    return retv215
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv217 Option__int
    var t218 func() Option__int = self__102.next_fn
    var t219 Option__int = t218()
    retv217 = t219
    return retv217
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv221 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t222 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__220,
        end_1: end__219,
    }
    var t223 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t222)
    })
    retv221 = t223
    return retv221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv225 *ref_int_x
    var t226 *ref_int_x = ref__Ref_3int(value__207)
    retv225 = t226
    return retv225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv228 int
    var t229 int = ref_get__Ref_3int(self__208)
    retv228 = t229
    return retv228
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv233 FnIterator__int
    var t234 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv233 = t234
    return retv233
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env118 closure_env_goml_builtin_range_0) Option__int {
    var retv239 Option__int
    var current__220 *ref_int_x = env118.current_0
    var end__219 int = env118.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t242 bool = value__221 < end__219
    var jp241 Option__int
    if t242 {
        var t243 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t243)
        var t244 Option__int = Some{
            _0: value__221,
        }
        jp241 = t244
    } else {
        jp241 = None{}
    }
    retv239 = jp241
    return retv239
}

func main() {
    main0()
}
