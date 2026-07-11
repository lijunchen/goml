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
    var retv34 int32
    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t36 int32 = t35 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t36)
    var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv34 = t37
    return retv34
}

func flip(flag__1 *ref_bool_x) bool {
    var retv39 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t40 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t40)
    var t41 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv39 = t41
    return retv39
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv43 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t44 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t44)
    var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t46 int32 = before__5 + t45
    retv43 = t46
    return retv43
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv48 int32
    var alias__7 *ref_int32_x = cell__6
    var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t50 int32 = t49 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t50)
    var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv48 = t51
    return retv48
}

func pair_sum() int32 {
    var retv53 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t55 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t56 int32 = t54 + t55
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t56)
    var t57 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t59 int32 = t57 + t58
    retv53 = t59
    return retv53
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv61 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t62 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t63 int32 = t62 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t63)
    var t64 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv61 = t64
    return retv61
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t66 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t66)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t67 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t68 int32 = t67 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t68)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t69 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t70 int32 = bumped__15 + t69
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t70)
    println__T_string(t71)
    var t72 int32 = nested_total_val__19 + alias_total__20
    var t73 int32 = t72 + reassigned__22
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    println__T_string(t74)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t75)
    var jp80 bool
    if flipped__16 {
        jp80 = flipped_again__17
    } else {
        jp80 = false
    }
    var jp77 bool
    if jp80 {
        jp77 = bool_check__23
    } else {
        jp77 = false
    }
    var t78 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp77)
    println__T_string(t78)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv82 int32
    var t83 int32 = ref_get__Ref_5int32(self__138)
    retv82 = t83
    return retv82
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__138 *ref_bool_x) bool {
    var retv87 bool
    var t88 bool = ref_get__Ref_4bool(self__138)
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__139 *ref_bool_x, value__140 bool) struct{} {
    ref_set__Ref_4bool(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__138 *ref_Ref_5int32_x) *ref_int32_x {
    var retv92 *ref_int32_x
    var t93 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__138)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv95 *ref_int32_x
    var t96 *ref_int32_x = ref__Ref_5int32(value__137)
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__137 bool) *ref_bool_x {
    var retv98 *ref_bool_x
    var t99 *ref_bool_x = ref__Ref_4bool(value__137)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__137 *ref_int32_x) *ref_Ref_5int32_x {
    var retv101 *ref_Ref_5int32_x
    var t102 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__137)
    retv101 = t102
    return retv101
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__2)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv110 string
    var t111 string = _goml_runtime_core_bool_to_string(self__8)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv113 string
    retv113 = self__9
    return retv113
}

func main() {
    main0()
}
