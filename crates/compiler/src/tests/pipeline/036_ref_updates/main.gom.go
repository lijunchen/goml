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
    var retv16 int32
    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var t18 int32 = t17 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, t18)
    var t19 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    retv16 = t19
    return retv16
}

func flip(flag__1 *ref_bool_x) bool {
    var retv21 bool
    var current__2 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    var t22 bool = !current__2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(flag__1, t22)
    var t23 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(flag__1)
    retv21 = t23
    return retv21
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var retv25 int32
    var inner__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(cell__3)
    var before__5 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t26 int32 = before__5 + 2
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__4, t26)
    var t27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__4)
    var t28 int32 = before__5 + t27
    retv25 = t28
    return retv25
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv30 int32
    var alias__7 *ref_int32_x = cell__6
    var t31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    var t32 int32 = t31 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(alias__7, t32)
    var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(alias__7)
    retv30 = t33
    return retv30
}

func pair_sum() int32 {
    var retv35 int32
    var first__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var second__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t38 int32 = t36 + t37
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(first__8, t38)
    var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(first__8)
    var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(second__9)
    var t41 int32 = t39 + t40
    retv35 = t41
    return retv35
}

func reassign_nested(nested__10 *ref_Ref_5int32_x) int32 {
    var retv43 int32
    var inner__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__10)
    var t44 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    var t45 int32 = t44 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__11, t45)
    var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__11)
    retv43 = t46
    return retv43
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t48 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t48)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inner__18)
    var t50 int32 = t49 + bumped__15
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inner__18, t50)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var t52 int32 = bumped__15 + t51
    var t53 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t52)
    println__T_string(t53)
    var t54 int32 = nested_total_val__19 + alias_total__20
    var t55 int32 = t54 + reassigned__22
    var t56 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t55)
    println__T_string(t56)
    var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(pair_total__21)
    println__T_string(t57)
    var jp62 bool
    if flipped__16 {
        jp62 = flipped_again__17
    } else {
        jp62 = false
    }
    var jp59 bool
    if jp62 {
        jp59 = bool_check__23
    } else {
        jp59 = false
    }
    var t60 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp59)
    println__T_string(t60)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv64 int32
    var t65 int32 = ref_get__Ref_5int32(self__103)
    retv64 = t65
    return retv64
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__103 *ref_bool_x) bool {
    var retv69 bool
    var t70 bool = ref_get__Ref_4bool(self__103)
    retv69 = t70
    return retv69
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__104 *ref_bool_x, value__105 bool) struct{} {
    ref_set__Ref_4bool(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__103 *ref_Ref_5int32_x) *ref_int32_x {
    var retv74 *ref_int32_x
    var t75 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__103)
    retv74 = t75
    return retv74
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv77 *ref_int32_x
    var t78 *ref_int32_x = ref__Ref_5int32(value__102)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__102 bool) *ref_bool_x {
    var retv80 *ref_bool_x
    var t81 *ref_bool_x = ref__Ref_4bool(value__102)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__102 *ref_int32_x) *ref_Ref_5int32_x {
    var retv83 *ref_Ref_5int32_x
    var t84 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__102)
    retv83 = t84
    return retv83
}

func println__T_string(value__1 string) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__2)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv92 string
    var t93 string = _goml_runtime_core_bool_to_string(self__8)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv95 string
    retv95 = self__9
    return retv95
}

func main() {
    main0()
}
