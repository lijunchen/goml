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
    var t80 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    println__T_bool(t80)
    var t81 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    println__T_bool(t81)
    var t82 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    println__T_bool(t82)
    var t83 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    println__T_bool(t83)
    var t84 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    println__T_bool(t84)
    var t85 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "")
    println__T_bool(t85)
    var t86 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    println__T_bool(t86)
    var t87 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    println__T_bool(t87)
    var t88 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    println__T_bool(t88)
    var t89 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    println__T_bool(t89)
    var t90 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    println__T_bool(t90)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__228 string, prefix__229 string) bool {
    var retv96 bool
    var t104 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
    var t105 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__228)
    var t106 bool = t104 <= t105
    var jp100 bool
    if t106 {
        var t107 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t108 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__228, t107)
        jp100 = t108
    } else {
        jp100 = false
    }
    var jp98 bool
    if jp100 {
        var t101 int = _goml_m_inherent_i_string_i_string_i_byte__len(prefix__229)
        var t102 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__228, 0, t101)
        var t103 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t102, prefix__229)
        jp98 = t103
    } else {
        jp98 = false
    }
    retv96 = jp98
    return retv96
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__230 string, suffix__231 string) bool {
    var retv110 bool
    var t113 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
    var t114 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
    var t115 bool = t113 > t114
    var jp112 bool
    if t115 {
        jp112 = false
    } else {
        var t116 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
        var t117 int = _goml_m_inherent_i_string_i_string_i_byte__len(suffix__231)
        var start__232 int = t116 - t117
        var t120 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__230, start__232)
        var jp119 bool
        if t120 {
            var t121 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__230)
            var t122 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__230, start__232, t121)
            var t123 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t122, suffix__231)
            jp119 = t123
        } else {
            jp119 = false
        }
        jp112 = jp119
    }
    retv110 = jp112
    return retv110
}

func _goml_m_inherent_i_string_i_string_i_contains(self__233 string, expected__234 string) bool {
    var retv125 bool
    var t128 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
    var t129 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t128, 0)
    var jp127 bool
    if t129 {
        jp127 = true
        retv125 = jp127
        return retv125
    } else {
        var t132 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
        var t133 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
        var t134 bool = t132 > t133
        var jp131 bool
        if t134 {
            jp131 = false
            jp127 = jp131
            retv125 = jp127
            return retv125
        } else {
            var t135 int = _goml_m_inherent_i_string_i_string_i_byte__len(self__233)
            var t136 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
            var t137 int = t135 - t136
            var t138 int = t137 + 1
            var t139 FnIterator__int = _goml_m_range(0, t138)
            var for_iter64 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t139)
            Loop_loop141:
            for {
                if true {
                    var for_next65 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter64)
                    switch for_next65.(type) {
                    case None:
                        break Loop_loop141
                    case Some:
                        var x66 int = for_next65.(Some)._0
                        var start__235 int = x66
                        var t143 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__234)
                        var end__236 int = start__235 + t143
                        var t151 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, start__235)
                        var jp148 bool
                        if t151 {
                            var t152 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__233, end__236)
                            jp148 = t152
                        } else {
                            jp148 = false
                        }
                        var jp146 bool
                        if jp148 {
                            var t149 string = _goml_m_inherent_i_string_i_string_i_byte__slice(self__233, start__235, end__236)
                            var t150 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t149, expected__234)
                            jp146 = t150
                        } else {
                            jp146 = false
                        }
                        if jp146 {
                            retv125 = true
                            return retv125
                        } else {
                            continue
                        }
                    default:
                        panic("non-exhaustive match")
                    }
                } else {
                    break Loop_loop141
                }
            }
            jp131 = false
            jp127 = jp131
            retv125 = jp127
            return retv125
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv154 string
    var t155 string = _goml_runtime_core_bool_to_string(self__37)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv157 int
    var t158 int = _goml_runtime_core_string_len(self__9)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv160 bool
    var t161 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv163 string
    var t164 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv166 bool
    var t167 bool = self__55 == other__56
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv169 bool
    var t170 bool = self__59 == other__60
    retv169 = t170
    return retv169
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv172 FnIterator__int
    var t173 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv172 = t173
    return retv172
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv175 FnIterator__int
    retv175 = self__109
    return retv175
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv177 Option__int
    var t178 func() Option__int = self__102.next_fn
    var t179 Option__int = t178()
    retv177 = t179
    return retv177
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv181 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t182 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__220,
        end_1: end__219,
    }
    var t183 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t182)
    })
    retv181 = t183
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv185 *ref_int_x
    var t186 *ref_int_x = ref__Ref_3int(value__207)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv188 int
    var t189 int = ref_get__Ref_3int(self__208)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv193 FnIterator__int
    var t194 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv193 = t194
    return retv193
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env78 closure_env_goml_builtin_range_0) Option__int {
    var retv199 Option__int
    var current__220 *ref_int_x = env78.current_0
    var end__219 int = env78.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t202 bool = value__221 < end__219
    var jp201 Option__int
    if t202 {
        var t203 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t203)
        var t204 Option__int = Some{
            _0: value__221,
        }
        jp201 = t204
    } else {
        jp201 = None{}
    }
    retv199 = jp201
    return retv199
}

func main() {
    main0()
}
