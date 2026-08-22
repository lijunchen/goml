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

type Ordering int32

func bump(cell__0 *ref_int32_x) int32 {
    var t424 int32
    var inline527 int32 = ref_get__Ref_5int32(cell__0)
    t424 = inline527
    var t425 int32 = t424 + 1
    ref_set__Ref_5int32(cell__0, t425)
    var inline523 int32 = ref_get__Ref_5int32(cell__0)
    return inline523
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline533 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline533
    var t429 bool = !current__2
    ref_set__Ref_4bool(flag__1, t429)
    var inline529 bool = ref_get__Ref_4bool(flag__1)
    return inline529
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline541 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline541
    var before__5 int32
    var inline539 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline539
    var t433 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t433)
    var t434 int32
    var inline535 int32 = ref_get__Ref_5int32(inner__4)
    t434 = inline535
    var t435 int32 = before__5 + t434
    return t435
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t455 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_i32_r_(t455)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(nested__14)
    var t456 int32
    var inline619 int32 = ref_get__Ref_5int32(inner__18)
    t456 = inline619
    var t457 int32 = t456 + bumped__15
    ref_set__Ref_5int32(inner__18, t457)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline612 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__12)
    var inline613 int32 = inline612 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(counter__12, inline613)
    var inline615 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__12)
    alias_total__20 = inline615
    var pair_total__21 int32
    var inline601 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(4)
    var inline602 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(6)
    var inline603 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline601)
    var inline604 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline602)
    var inline605 int32 = inline603 + inline604
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline601, inline605)
    var inline607 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline601)
    var inline608 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline602)
    var inline609 int32 = inline607 + inline608
    pair_total__21 = inline609
    var reassigned__22 int32
    var inline595 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(nested__14)
    var inline596 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline595)
    var inline597 int32 = inline596 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline595, inline597)
    var inline599 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline595)
    reassigned__22 = inline599
    var bool_check__23 bool = !false
    var t458 int32
    var inline593 int32 = ref_get__Ref_5int32(counter__12)
    t458 = inline593
    var t459 int32 = bumped__15 + t458
    var t460 string
    var inline591 string = _goml_runtime_core_int32_to_string(t459)
    t460 = inline591
    var inline588 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t460)
    _goml_runtime_core_string_println(inline588)
    var t461 int32 = nested_total_val__19 + alias_total__20
    var t462 int32 = t461 + reassigned__22
    var t463 string
    var inline586 string = _goml_runtime_core_int32_to_string(t462)
    t463 = inline586
    var inline583 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t463)
    _goml_runtime_core_string_println(inline583)
    var t464 string
    var inline581 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t464 = inline581
    var inline578 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline578)
    var jp469 bool
    if flipped__16 {
        jp469 = flipped_again__17
    } else {
        jp469 = false
    }
    var jp466 bool
    if jp469 {
        jp466 = bool_check__23
    } else {
        jp466 = false
    }
    var t467 string
    var inline576 string = _goml_runtime_core_bool_to_string(jp466)
    t467 = inline576
    var inline573 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline573)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__432 *ref_int32_x) int32 {
    var t472 int32 = ref_get__Ref_5int32(self__432)
    return t472
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(self__432 *ref_Ref_5int32_x) *ref_int32_x {
    var t482 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__432)
    return t482
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__431 int32) *ref_int32_x {
    var t485 *ref_int32_x = ref__Ref_5int32(value__431)
    return t485
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__431 bool) *ref_bool_x {
    var t488 *ref_bool_x = ref__Ref_4bool(value__431)
    return t488
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_i32_r_(value__431 *ref_int32_x) *ref_Ref_5int32_x {
    var t491 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__431)
    return t491
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
