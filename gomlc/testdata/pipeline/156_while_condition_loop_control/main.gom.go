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
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop440:
    for {
        var t451 int
        var inline498 int = ref_get__Ref_3int(i__0)
        t451 = inline498
        var t452 bool = t451 == 0
        var jp442 bool
        if t452 {
            var inline478 int = 1
            ref_set__Ref_3int(i__0, inline478)
            jp442 = true
        } else {
            var t455 int
            var inline481 int = ref_get__Ref_3int(i__0)
            t455 = inline481
            var t456 bool = t455 < 4
            if t456 {
                jp442 = true
            } else {
                jp442 = false
            }
        }
        if jp442 {
            var t443 int
            var inline496 int = ref_get__Ref_3int(total__1)
            t443 = inline496
            var t444 int
            var inline494 int = ref_get__Ref_3int(i__0)
            t444 = inline494
            var t445 int = t443 + t444
            ref_set__Ref_3int(total__1, t445)
            var t449 int
            var inline490 int = ref_get__Ref_3int(i__0)
            t449 = inline490
            var t450 bool = t449 == 1
            if t450 {
                var inline483 int = 2
                ref_set__Ref_3int(i__0, inline483)
                continue
            } else {
                var t447 int
                var inline488 int = ref_get__Ref_3int(i__0)
                t447 = inline488
                var t448 int = t447 + 1
                ref_set__Ref_3int(i__0, t448)
                continue
            }
        } else {
            break Loop_loop440
        }
    }
    var t428 int
    var inline530 int = ref_get__Ref_3int(total__1)
    t428 = inline530
    var inline527 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t428)
    _goml_runtime_core_string_println(inline527)
    var j__2 *ref_int_x
    var inline524 int = 0
    var inline525 *ref_int_x = ref__Ref_3int(inline524)
    j__2 = inline525
    var total2__3 *ref_int_x
    var inline521 int = 0
    var inline522 *ref_int_x = ref__Ref_3int(inline521)
    total2__3 = inline522
    Loop_loop431:
    for {
        var mtmp418 int
        var inline514 int = ref_get__Ref_3int(j__2)
        mtmp418 = inline514
        var jp433 bool
        switch mtmp418 {
        case 0:
            var inline500 int = 1
            ref_set__Ref_3int(j__2, inline500)
            jp433 = true
        case 1:
            var inline503 int = 2
            ref_set__Ref_3int(j__2, inline503)
            jp433 = true
        case 2:
            jp433 = true
        default:
            jp433 = false
        }
        if jp433 {
            var t434 int
            var inline512 int = ref_get__Ref_3int(total2__3)
            t434 = inline512
            var t435 int
            var inline510 int = ref_get__Ref_3int(j__2)
            t435 = inline510
            var t436 int = t434 + t435
            ref_set__Ref_3int(total2__3, t436)
            var t438 int
            var inline506 int = ref_get__Ref_3int(j__2)
            t438 = inline506
            var t439 bool = t438 == 2
            if t439 {
                break Loop_loop431
            } else {
                continue
            }
        } else {
            break Loop_loop431
        }
    }
    var t430 int
    var inline519 int = ref_get__Ref_3int(total2__3)
    t430 = inline519
    var inline516 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t430)
    _goml_runtime_core_string_println(inline516)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t459 *ref_int_x = ref__Ref_3int(value__431)
    return t459
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t470 string = _goml_runtime_core_int_to_string(self__151)
    return t470
}

func main() {
    main0()
}
