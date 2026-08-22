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
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop457:
    for {
        var t463 int
        var inline495 int = ref_get__Ref_3int(i__0)
        t463 = inline495
        var t464 bool = t463 < 3
        var jp459 bool
        if t464 {
            jp459 = true
        } else {
            jp459 = false
        }
        if jp459 {
            var t460 int
            var inline493 int = ref_get__Ref_3int(i__0)
            t460 = inline493
            var inline490 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t460)
            _goml_runtime_core_string_println(inline490)
            var t461 int
            var inline488 int = ref_get__Ref_3int(i__0)
            t461 = inline488
            var t462 int = t461 + 1
            ref_set__Ref_3int(i__0, t462)
            continue
        } else {
            break Loop_loop457
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop441:
    for {
        var t449 int
        var inline511 int = ref_get__Ref_3int(j__1)
        t449 = inline511
        var t450 bool = t449 < 4
        var jp443 bool
        if t450 {
            var t453 int
            var inline499 int = ref_get__Ref_3int(j__1)
            t453 = inline499
            var t454 bool = t453 == 1
            if t454 {
                jp443 = true
            } else {
                var t455 int
                var inline497 int = ref_get__Ref_3int(j__1)
                t455 = inline497
                var t456 bool = t455 != 3
                jp443 = t456
            }
        } else {
            jp443 = false
        }
        if jp443 {
            var t444 int
            var inline509 int = ref_get__Ref_3int(total__2)
            t444 = inline509
            var t445 int
            var inline507 int = ref_get__Ref_3int(j__1)
            t445 = inline507
            var t446 int = t444 + t445
            ref_set__Ref_3int(total__2, t446)
            var t447 int
            var inline503 int = ref_get__Ref_3int(j__1)
            t447 = inline503
            var t448 int = t447 + 1
            ref_set__Ref_3int(j__1, t448)
            continue
        } else {
            break Loop_loop441
        }
    }
    var t426 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(total__2)
    println__T_isize(t426)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var sum__4 *ref_int_x
    var inline532 int = 0
    var inline533 *ref_int_x = ref__Ref_3int(inline532)
    sum__4 = inline533
    Loop_loop429:
    for {
        var mtmp418 int
        var inline525 int = ref_get__Ref_3int(k__3)
        mtmp418 = inline525
        var jp431 bool
        switch mtmp418 {
        case 0:
            jp431 = true
        case 1:
            var t439 int
            var inline513 int = ref_get__Ref_3int(sum__4)
            t439 = inline513
            var t440 bool = t439 == 0
            if t440 {
                jp431 = true
            } else {
                jp431 = false
            }
        case 2:
            jp431 = true
        default:
            jp431 = false
        }
        if jp431 {
            var t432 int
            var inline523 int = ref_get__Ref_3int(sum__4)
            t432 = inline523
            var t433 int
            var inline521 int = ref_get__Ref_3int(k__3)
            t433 = inline521
            var t434 int = t432 + t433
            ref_set__Ref_3int(sum__4, t434)
            var t435 int
            var inline517 int = ref_get__Ref_3int(k__3)
            t435 = inline517
            var t436 int = t435 + 1
            ref_set__Ref_3int(k__3, t436)
            continue
        } else {
            break Loop_loop429
        }
    }
    var t428 int
    var inline530 int = ref_get__Ref_3int(sum__4)
    t428 = inline530
    var inline527 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t428)
    _goml_runtime_core_string_println(inline527)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t467 *ref_int_x = ref__Ref_3int(value__431)
    return t467
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__432 *ref_int_x) int {
    var t470 int = ref_get__Ref_3int(self__432)
    return t470
}

func println__T_isize(value__1 int) struct{} {
    var t472 string
    var inline535 string = _goml_runtime_core_int_to_string(value__1)
    t472 = inline535
    _goml_runtime_core_string_println(t472)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t478 string = _goml_runtime_core_int_to_string(self__151)
    return t478
}

func main() {
    main0()
}
