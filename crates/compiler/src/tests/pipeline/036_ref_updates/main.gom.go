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
    var retv70 int32
    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t72 int32 = t71 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t72)
    var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv70 = t73
    return retv70
}

func flip(flag__1 *ref_bool_x) bool {
    var retv75 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t76 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t76)
    var t77 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv75 = t77
    return retv75
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv79 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t80 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t80)
    var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t82 int32 = before__5 + t81
    retv79 = t82
    return retv79
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv84 int32
    var alias__7 *ref_int32_x = cell__6
    var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t86 int32 = t85 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t86)
    var t87 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv84 = t87
    return retv84
}

func pair_sum() int32 {
    var retv89 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t92 int32 = t90 + t91
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t92)
    var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t95 int32 = t93 + t94
    retv89 = t95
    return retv89
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv97 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t98 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t99 int32 = t98 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t99)
    var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv97 = t100
    return retv97
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t102 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t102)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t104 int32 = t103 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t104)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t106 int32 = bumped__15 + t105
    var t107 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t106)
    println__T_string(t107)
    var t108 int32 = nested_total_val__19 + alias_total__20
    var t109 int32 = t108 + reassigned__22
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t109)
    println__T_string(t110)
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t111)
    var jp116 bool
    if flipped__16 {
        jp116 = flipped_again__17
    } else {
        jp116 = false
    }
    var jp113 bool
    if jp116 {
        jp113 = bool_check__23
    } else {
        jp113 = false
    }
    var t114 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp113)
    println__T_string(t114)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv118 int32
    var t119 int32 = ref_get__Ref_5int32(self__201)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__201 *ref_bool_x) bool {
    var retv123 bool
    var t124 bool = ref_get__Ref_4bool(self__201)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__202 *ref_bool_x, value__203 bool) struct{} {
    ref_set__Ref_4bool(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__201 *ref_Ref_5int32_x) *ref_int32_x {
    var retv128 *ref_int32_x
    var t129 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__201)
    retv128 = t129
    return retv128
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv131 *ref_int32_x
    var t132 *ref_int32_x = ref__Ref_5int32(value__200)
    retv131 = t132
    return retv131
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__200 bool) *ref_bool_x {
    var retv134 *ref_bool_x
    var t135 *ref_bool_x = ref__Ref_4bool(value__200)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__200 *ref_int32_x) *ref_Ref_5int32_x {
    var retv137 *ref_Ref_5int32_x
    var t138 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__200)
    retv137 = t138
    return retv137
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_int32_to_string(self__2)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv146 string
    var t147 string = _goml_runtime_core_bool_to_string(self__33)
    retv146 = t147
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv149 string
    retv149 = self__34
    return retv149
}

func main() {
    main0()
}
