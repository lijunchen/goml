package main

import (
    _goml_fmt "fmt"
)

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

type Ordering int32

type Result__int32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var mtmp412 Result__int32__string
    var inline484 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline485 int32 = inline484 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline485)
    if ok__3 {
        var inline487 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline488 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: inline487,
        }
        mtmp412 = inline488
    } else {
        var inline489 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: "bump failed",
        }
        mtmp412 = inline489
    }
    var jp431 int32
    switch mtmp412._tag {
    case 0:
        var x413 int32 = mtmp412._v0_0
        jp431 = x413
        var t432 int32
        var inline482 int32 = ref_get__Ref_5int32(counter__2)
        t432 = inline482
        var t433 int32 = jp431 + t432
        var t434 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: t433,
        }
        return t434
    case 1:
        var x414 string = mtmp412._v1_0
        var t435 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: x414,
        }
        return t435
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5._tag {
    case 0:
        var x415 int32 = res__5._v0_0
        var t440 string
        var inline491 string = _goml_runtime_core_int32_to_string(x415)
        t440 = inline491
        var t441 string = "ok " + t440
        return t441
    case 1:
        var x416 string = res__5._v1_0
        var t442 string = "err " + x416
        return t442
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t451 string
    var inline536 bool = true
    var inline537 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline538 Result__int32__string = use_try(inline537, inline536)
    var inline539 string = show(inline538)
    var inline540 string = inline539 + " count="
    var inline541 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline537)
    var inline542 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline541)
    var inline543 string = inline540 + inline542
    t451 = inline543
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t451)
    _goml_runtime_core_string_println(inline533)
    var t452 string
    var inline524 bool = false
    var inline525 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline526 Result__int32__string = use_try(inline525, inline524)
    var inline527 string = show(inline526)
    var inline528 string = inline527 + " count="
    var inline529 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline525)
    var inline530 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline529)
    var inline531 string = inline528 + inline530
    t452 = inline531
    var inline521 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline521)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t455 int32 = ref_get__Ref_5int32(self__432)
    return t455
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t460 string = _goml_runtime_core_int32_to_string(self__33)
    return t460
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t463 *ref_int32_x = ref__Ref_5int32(value__431)
    return t463
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
