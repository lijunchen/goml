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
    var t421 int32
    var inline524 int32 = ref_get__Ref_5int32(cell__0)
    t421 = inline524
    var t422 int32 = t421 + 1
    ref_set__Ref_5int32(cell__0, t422)
    var inline520 int32 = ref_get__Ref_5int32(cell__0)
    return inline520
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline530 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline530
    var t426 bool = !current__2
    ref_set__Ref_4bool(flag__1, t426)
    var inline526 bool = ref_get__Ref_4bool(flag__1)
    return inline526
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline538 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline538
    var before__5 int32
    var inline536 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline536
    var t430 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t430)
    var t431 int32
    var inline532 int32 = ref_get__Ref_5int32(inner__4)
    t431 = inline532
    var t432 int32 = before__5 + t431
    return t432
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t452 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(t452)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var t453 int32
    var inline616 int32 = ref_get__Ref_5int32(inner__18)
    t453 = inline616
    var t454 int32 = t453 + bumped__15
    ref_set__Ref_5int32(inner__18, t454)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline609 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    var inline610 int32 = inline609 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__12, inline610)
    var inline612 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__12)
    alias_total__20 = inline612
    var pair_total__21 int32
    var inline598 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(4)
    var inline599 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(6)
    var inline600 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline598)
    var inline601 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline599)
    var inline602 int32 = inline600 + inline601
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline598, inline602)
    var inline604 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline598)
    var inline605 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline599)
    var inline606 int32 = inline604 + inline605
    pair_total__21 = inline606
    var reassigned__22 int32
    var inline592 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(nested__14)
    var inline593 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline592)
    var inline594 int32 = inline593 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline592, inline594)
    var inline596 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline592)
    reassigned__22 = inline596
    var bool_check__23 bool = !false
    var t455 int32
    var inline590 int32 = ref_get__Ref_5int32(counter__12)
    t455 = inline590
    var t456 int32 = bumped__15 + t455
    var t457 string
    var inline588 string = _goml_runtime_core_int32_to_string(t456)
    t457 = inline588
    var inline585 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline585)
    var t458 int32 = nested_total_val__19 + alias_total__20
    var t459 int32 = t458 + reassigned__22
    var t460 string
    var inline583 string = _goml_runtime_core_int32_to_string(t459)
    t460 = inline583
    var inline580 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t460)
    _goml_runtime_core_string_println(inline580)
    var t461 string
    var inline578 string = _goml_runtime_core_int32_to_string(pair_total__21)
    t461 = inline578
    var inline575 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline575)
    var jp466 bool
    if flipped__16 {
        jp466 = flipped_again__17
    } else {
        jp466 = false
    }
    var jp463 bool
    if jp466 {
        jp463 = bool_check__23
    } else {
        jp463 = false
    }
    var t464 string
    var inline573 string = _goml_runtime_core_bool_to_string(jp463)
    t464 = inline573
    var inline570 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline570)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t469 int32 = ref_get__Ref_5int32(self__432)
    return t469
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_int32_r_(self__432 *ref_Ref_5int32_x) *ref_int32_x {
    var t479 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__432)
    return t479
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t482 *ref_int32_x = ref__Ref_5int32(value__431)
    return t482
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__431 bool) *ref_bool_x {
    var t485 *ref_bool_x = ref__Ref_4bool(value__431)
    return t485
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_int32_r_(value__431 *ref_int32_x) *ref_Ref_5int32_x {
    var t488 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__431)
    return t488
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
