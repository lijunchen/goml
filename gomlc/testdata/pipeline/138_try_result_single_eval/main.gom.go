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

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var mtmp409 Result__int32__string
    var inline481 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline482 int32 = inline481 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline482)
    if ok__3 {
        var inline484 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline485 Result__int32__string = Ok{
            _0: inline484,
        }
        mtmp409 = inline485
    } else {
        var inline486 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp409 = inline486
    }
    var jp428 int32
    switch mtmp409.(type) {
    case Ok:
        var x410 int32 = mtmp409.(Ok)._0
        jp428 = x410
        var t429 int32
        var inline479 int32 = ref_get__Ref_5int32(counter__2)
        t429 = inline479
        var t430 int32 = jp428 + t429
        var t431 Result__int32__string = Ok{
            _0: t430,
        }
        return t431
    case Err:
        var x411 string = mtmp409.(Err)._0
        var t432 Result__int32__string = Err{
            _0: x411,
        }
        return t432
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x412 int32 = res__5.(Ok)._0
        var t437 string
        var inline488 string = _goml_runtime_core_int32_to_string(x412)
        t437 = inline488
        var t438 string = "ok " + t437
        return t438
    case Err:
        var x413 string = res__5.(Err)._0
        var t439 string = "err " + x413
        return t439
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t448 string
    var inline533 bool = true
    var inline534 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline535 Result__int32__string = use_try(inline534, inline533)
    var inline536 string = show(inline535)
    var inline537 string = inline536 + " count="
    var inline538 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline534)
    var inline539 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline538)
    var inline540 string = inline537 + inline539
    t448 = inline540
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline530)
    var t449 string
    var inline521 bool = false
    var inline522 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline523 Result__int32__string = use_try(inline522, inline521)
    var inline524 string = show(inline523)
    var inline525 string = inline524 + " count="
    var inline526 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline522)
    var inline527 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline526)
    var inline528 string = inline525 + inline527
    t449 = inline528
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline518)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t452 int32 = ref_get__Ref_5int32(self__432)
    return t452
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t457 string = _goml_runtime_core_int32_to_string(self__33)
    return t457
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t460 *ref_int32_x = ref__Ref_5int32(value__431)
    return t460
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
