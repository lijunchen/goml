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
    Loop_loop454:
    for {
        var t460 int
        var inline492 int = ref_get__Ref_3int(i__0)
        t460 = inline492
        var t461 bool = t460 < 3
        var jp456 bool
        if t461 {
            jp456 = true
        } else {
            jp456 = false
        }
        if jp456 {
            var t457 int
            var inline490 int = ref_get__Ref_3int(i__0)
            t457 = inline490
            var inline487 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t457)
            _goml_runtime_core_string_println(inline487)
            var t458 int
            var inline485 int = ref_get__Ref_3int(i__0)
            t458 = inline485
            var t459 int = t458 + 1
            ref_set__Ref_3int(i__0, t459)
            continue
        } else {
            break Loop_loop454
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop438:
    for {
        var t446 int
        var inline508 int = ref_get__Ref_3int(j__1)
        t446 = inline508
        var t447 bool = t446 < 4
        var jp440 bool
        if t447 {
            var t450 int
            var inline496 int = ref_get__Ref_3int(j__1)
            t450 = inline496
            var t451 bool = t450 == 1
            if t451 {
                jp440 = true
            } else {
                var t452 int
                var inline494 int = ref_get__Ref_3int(j__1)
                t452 = inline494
                var t453 bool = t452 != 3
                jp440 = t453
            }
        } else {
            jp440 = false
        }
        if jp440 {
            var t441 int
            var inline506 int = ref_get__Ref_3int(total__2)
            t441 = inline506
            var t442 int
            var inline504 int = ref_get__Ref_3int(j__1)
            t442 = inline504
            var t443 int = t441 + t442
            ref_set__Ref_3int(total__2, t443)
            var t444 int
            var inline500 int = ref_get__Ref_3int(j__1)
            t444 = inline500
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
    var sum__4 *ref_int_x
    var inline529 int = 0
    var inline530 *ref_int_x = ref__Ref_3int(inline529)
    sum__4 = inline530
    Loop_loop426:
    for {
        var mtmp415 int
        var inline522 int = ref_get__Ref_3int(k__3)
        mtmp415 = inline522
        var jp428 bool
        switch mtmp415 {
        case 0:
            jp428 = true
        case 1:
            var t436 int
            var inline510 int = ref_get__Ref_3int(sum__4)
            t436 = inline510
            var t437 bool = t436 == 0
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
            var inline520 int = ref_get__Ref_3int(sum__4)
            t429 = inline520
            var t430 int
            var inline518 int = ref_get__Ref_3int(k__3)
            t430 = inline518
            var t431 int = t429 + t430
            ref_set__Ref_3int(sum__4, t431)
            var t432 int
            var inline514 int = ref_get__Ref_3int(k__3)
            t432 = inline514
            var t433 int = t432 + 1
            ref_set__Ref_3int(k__3, t433)
            continue
        } else {
            break Loop_loop426
        }
    }
    var t425 int
    var inline527 int = ref_get__Ref_3int(sum__4)
    t425 = inline527
    var inline524 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t425)
    _goml_runtime_core_string_println(inline524)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t464 *ref_int_x = ref__Ref_3int(value__431)
    return t464
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t467 int = ref_get__Ref_3int(self__432)
    return t467
}

func println__T_int(value__1 int) struct{} {
    var t469 string
    var inline532 string = _goml_runtime_core_int_to_string(value__1)
    t469 = inline532
    _goml_runtime_core_string_println(t469)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t475 string = _goml_runtime_core_int_to_string(self__151)
    return t475
}

func main() {
    main0()
}
