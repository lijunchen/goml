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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Ref_5int32_x struct {
    value *ref_int32_x
}

func ref__Ref_10Ref_5int32(value *ref_int32_x) *ref_Ref_5int32_x {
    return &ref_Ref_5int32_x{
        value: value,
    }
}

func ref_get__Ref_10Ref_5int32(reference *ref_Ref_5int32_x) *ref_int32_x {
    return reference.value
}

func bump(cell__0 *ref_int32_x) int32 {
    var retv120 int32
    var t121 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t122 int32 = t121 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t122)
    var t123 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv120 = t123
    return retv120
}

func flip(flag__1 *ref_bool_x) bool {
    var retv125 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t126 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t126)
    var t127 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv125 = t127
    return retv125
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv129 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t130 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t130)
    var t131 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t132 int32 = before__5 + t131
    retv129 = t132
    return retv129
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv134 int32
    var alias__7 *ref_int32_x = cell__6
    var t135 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t136 int32 = t135 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t136)
    var t137 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv134 = t137
    return retv134
}

func pair_sum() int32 {
    var retv139 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t142 int32 = t140 + t141
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t142)
    var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t145 int32 = t143 + t144
    retv139 = t145
    return retv139
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv147 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t148 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t149 int32 = t148 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t149)
    var t150 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv147 = t150
    return retv147
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t152 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t152)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t153 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t154 int32 = t153 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t154)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t155 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t156 int32 = bumped__15 + t155
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t156)
    println__T_string(t157)
    var t158 int32 = nested_total_val__19 + alias_total__20
    var t159 int32 = t158 + reassigned__22
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t159)
    println__T_string(t160)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t161)
    var jp166 bool
    if flipped__16 {
        jp166 = flipped_again__17
    } else {
        jp166 = false
    }
    var jp163 bool
    if jp166 {
        jp163 = bool_check__23
    } else {
        jp163 = false
    }
    var t164 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp163)
    println__T_string(t164)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv168 int32
    var t169 int32 = ref_get__Ref_5int32(self__208)
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv173 bool
    var t174 bool = ref_get__Ref_4bool(self__208)
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__208 *ref_Ref_5int32_x) *ref_int32_x {
    var retv178 *ref_int32_x
    var t179 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__208)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv181 *ref_int32_x
    var t182 *ref_int32_x = ref__Ref_5int32(value__207)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv184 *ref_bool_x
    var t185 *ref_bool_x = ref__Ref_4bool(value__207)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__207 *ref_int32_x) *ref_Ref_5int32_x {
    var retv187 *ref_Ref_5int32_x
    var t188 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__207)
    retv187 = t188
    return retv187
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv193 string
    var t194 string = _goml_runtime_core_int32_to_string(self__6)
    retv193 = t194
    return retv193
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv196 string
    var t197 string = _goml_runtime_core_bool_to_string(self__37)
    retv196 = t197
    return retv196
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv199 string
    retv199 = self__38
    return retv199
}

func main() {
    main0()
}
