package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop455:
    for {
        var t461 int
        var inline493 int = ref_get__Ref_3int(i__0)
        t461 = inline493
        var t462 bool = t461 < 3
        var jp457 bool
        if t462 {
            jp457 = true
        } else {
            jp457 = false
        }
        if jp457 {
            var t458 int
            var inline491 int = ref_get__Ref_3int(i__0)
            t458 = inline491
            var inline488 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t458)
            _goml_runtime_core_string_println(inline488)
            var t459 int
            var inline486 int = ref_get__Ref_3int(i__0)
            t459 = inline486
            var t460 int = t459 + 1
            ref_set__Ref_3int(i__0, t460)
            continue
        } else {
            break Loop_loop455
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop438:
    for {
        var t446 int
        var inline515 int = ref_get__Ref_3int(j__1)
        t446 = inline515
        var t447 bool = t446 < 4
        var jp440 bool
        if t447 {
            var t450 int
            var inline503 int = ref_get__Ref_3int(j__1)
            t450 = inline503
            var t451 bool
            var inline500 int = 1
            var inline501 bool = t450 == inline500
            t451 = inline501
            if t451 {
                jp440 = true
            } else {
                var t452 int
                var inline498 int = ref_get__Ref_3int(j__1)
                t452 = inline498
                var t453 bool
                var inline495 int = 3
                var inline496 bool = t452 == inline495
                t453 = inline496
                var t454 bool = !t453
                jp440 = t454
            }
        } else {
            jp440 = false
        }
        if jp440 {
            var t441 int
            var inline513 int = ref_get__Ref_3int(total__2)
            t441 = inline513
            var t442 int
            var inline511 int = ref_get__Ref_3int(j__1)
            t442 = inline511
            var t443 int = t441 + t442
            ref_set__Ref_3int(total__2, t443)
            var t444 int
            var inline507 int = ref_get__Ref_3int(j__1)
            t444 = inline507
            var t445 int = t444 + 1
            ref_set__Ref_3int(j__1, t445)
            continue
        } else {
            break Loop_loop438
        }
    }
    var t423 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t423)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop426:
    for {
        var mtmp415 int
        var inline532 int = ref_get__Ref_3int(k__3)
        mtmp415 = inline532
        var jp428 bool
        switch mtmp415 {
        case 0:
            jp428 = true
        case 1:
            var t436 int
            var inline520 int = ref_get__Ref_3int(sum__4)
            t436 = inline520
            var t437 bool
            var inline517 int = 0
            var inline518 bool = t436 == inline517
            t437 = inline518
            if t437 {
                jp428 = true
            } else {
                jp428 = false
            }
        case 2:
            jp428 = true
        default:
            jp428 = false
        }
        if jp428 {
            var t429 int
            var inline530 int = ref_get__Ref_3int(sum__4)
            t429 = inline530
            var t430 int
            var inline528 int = ref_get__Ref_3int(k__3)
            t430 = inline528
            var t431 int = t429 + t430
            ref_set__Ref_3int(sum__4, t431)
            var t432 int
            var inline524 int = ref_get__Ref_3int(k__3)
            t432 = inline524
            var t433 int = t432 + 1
            ref_set__Ref_3int(k__3, t433)
            continue
        } else {
            break Loop_loop426
        }
    }
    var t425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t425)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t465 *ref_int_x = ref__Ref_3int(value__431)
    return t465
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t468 int = ref_get__Ref_3int(self__432)
    return t468
}

func println__T_int(value__1 int) struct{} {
    var t470 string
    var inline534 string = _goml_runtime_core_int_to_string(value__1)
    t470 = inline534
    _goml_runtime_core_string_println(t470)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t479 string = _goml_runtime_core_int_to_string(self__151)
    return t479
}

func main() {
    main0()
}
