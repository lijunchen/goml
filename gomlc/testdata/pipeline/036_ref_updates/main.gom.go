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
    var retv76 int32
    var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t78 int32 = t77 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t78)
    var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv76 = t79
    return retv76
}

func flip(flag__1 *ref_bool_x) bool {
    var retv81 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t82 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t82)
    var t83 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv81 = t83
    return retv81
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv85 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t86 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t86)
    var t87 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t88 int32 = before__5 + t87
    retv85 = t88
    return retv85
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv90 int32
    var alias__7 *ref_int32_x = cell__6
    var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t92 int32 = t91 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t92)
    var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv90 = t93
    return retv90
}

func pair_sum() int32 {
    var retv95 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t98 int32 = t96 + t97
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t98)
    var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t101 int32 = t99 + t100
    retv95 = t101
    return retv95
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv103 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t105 int32 = t104 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t105)
    var t106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv103 = t106
    return retv103
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t108)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t109 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t110 int32 = t109 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t110)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t112 int32 = bumped__15 + t111
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t112)
    println__T_string(t113)
    var t114 int32 = nested_total_val__19 + alias_total__20
    var t115 int32 = t114 + reassigned__22
    var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t115)
    println__T_string(t116)
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t117)
    var jp122 bool
    if flipped__16 {
        jp122 = flipped_again__17
    } else {
        jp122 = false
    }
    var jp119 bool
    if jp122 {
        jp119 = bool_check__23
    } else {
        jp119 = false
    }
    var t120 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp119)
    println__T_string(t120)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv124 int32
    var t125 int32 = ref_get__Ref_5int32(self__210)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv129 bool
    var t130 bool = ref_get__Ref_4bool(self__210)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__210 *ref_Ref_5int32_x) *ref_int32_x {
    var retv134 *ref_int32_x
    var t135 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__210)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv137 *ref_int32_x
    var t138 *ref_int32_x = ref__Ref_5int32(value__209)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv140 *ref_bool_x
    var t141 *ref_bool_x = ref__Ref_4bool(value__209)
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__209 *ref_int32_x) *ref_Ref_5int32_x {
    var retv143 *ref_Ref_5int32_x
    var t144 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__209)
    retv143 = t144
    return retv143
}

func println__T_string(value__1 string) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv149 string
    var t150 string = _goml_runtime_core_int32_to_string(self__6)
    retv149 = t150
    return retv149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv152 string
    var t153 string = _goml_runtime_core_bool_to_string(self__37)
    retv152 = t153
    return retv152
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv155 string
    retv155 = self__38
    return retv155
}

func main() {
    main0()
}
