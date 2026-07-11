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
    var retv19 int32
    var t20 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t21 int32 = t20 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t21)
    var t22 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv19 = t22
    return retv19
}

func flip(flag__1 *ref_bool_x) bool {
    var retv24 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t25 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t25)
    var t26 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv24 = t26
    return retv24
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv28 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t29 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t29)
    var t30 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t31 int32 = before__5 + t30
    retv28 = t31
    return retv28
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv33 int32
    var alias__7 *ref_int32_x = cell__6
    var t34 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t35 int32 = t34 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t35)
    var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv33 = t36
    return retv33
}

func pair_sum() int32 {
    var retv38 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t41 int32 = t39 + t40
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t41)
    var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t44 int32 = t42 + t43
    retv38 = t44
    return retv38
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv46 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t48 int32 = t47 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t48)
    var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv46 = t49
    return retv46
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t51)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t52 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t53 int32 = t52 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t53)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t55 int32 = bumped__15 + t54
    var t56 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t55)
    println__T_string(t56)
    var t57 int32 = nested_total_val__19 + alias_total__20
    var t58 int32 = t57 + reassigned__22
    var t59 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t58)
    println__T_string(t59)
    var t60 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t60)
    var jp65 bool
    if flipped__16 {
        jp65 = flipped_again__17
    } else {
        jp65 = false
    }
    var jp62 bool
    if jp65 {
        jp62 = bool_check__23
    } else {
        jp62 = false
    }
    var t63 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp62)
    println__T_string(t63)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv67 int32
    var t68 int32 = ref_get__Ref_5int32(self__115)
    retv67 = t68
    return retv67
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__115 *ref_bool_x) bool {
    var retv72 bool
    var t73 bool = ref_get__Ref_4bool(self__115)
    retv72 = t73
    return retv72
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__116 *ref_bool_x, value__117 bool) struct{} {
    ref_set__Ref_4bool(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__115 *ref_Ref_5int32_x) *ref_int32_x {
    var retv77 *ref_int32_x
    var t78 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__115)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv80 *ref_int32_x
    var t81 *ref_int32_x = ref__Ref_5int32(value__114)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__114 bool) *ref_bool_x {
    var retv83 *ref_bool_x
    var t84 *ref_bool_x = ref__Ref_4bool(value__114)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__114 *ref_int32_x) *ref_Ref_5int32_x {
    var retv86 *ref_Ref_5int32_x
    var t87 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__114)
    retv86 = t87
    return retv86
}

func println__T_string(value__1 string) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int32_to_string(self__2)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv95 string
    var t96 string = _goml_runtime_core_bool_to_string(self__8)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv98 string
    retv98 = self__9
    return retv98
}

func main() {
    main0()
}
