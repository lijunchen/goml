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
    var i__0 *ref_int_x
    var inline511 int = 0
    var inline512 *ref_int_x = ref__Ref_3int(inline511)
    i__0 = inline512
    var sum__1 *ref_int_x
    var inline508 int = 0
    var inline509 *ref_int_x = ref__Ref_3int(inline508)
    sum__1 = inline509
    Loop_loop430:
    for {
        var t431 int
        var inline478 int = ref_get__Ref_3int(i__0)
        t431 = inline478
        var t432 bool = t431 < 5
        if t432 {
            var t433 int
            var inline476 int = ref_get__Ref_3int(i__0)
            t433 = inline476
            var t434 int = t433 + 1
            ref_set__Ref_3int(i__0, t434)
            var t439 int
            var inline472 int = ref_get__Ref_3int(i__0)
            t439 = inline472
            var t440 bool
            var inline469 int = 3
            var inline470 bool = t439 == inline469
            t440 = inline470
            var jp436 int
            if t440 {
                continue
            } else {
                var inline463 int = ref_get__Ref_3int(i__0)
                jp436 = inline463
                var t437 int
                var inline467 int = ref_get__Ref_3int(sum__1)
                t437 = inline467
                var t438 int = t437 + jp436
                ref_set__Ref_3int(sum__1, t438)
                continue
            }
        } else {
            break Loop_loop430
        }
    }
    var t419 int
    var inline506 int = ref_get__Ref_3int(sum__1)
    t419 = inline506
    var inline503 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t419)
    _goml_runtime_core_string_println(inline503)
    var j__3 *ref_int_x
    var inline500 int = 0
    var inline501 *ref_int_x = ref__Ref_3int(inline500)
    j__3 = inline501
    var total__4 *ref_int_x
    var inline497 int = 0
    var inline498 *ref_int_x = ref__Ref_3int(inline497)
    total__4 = inline498
    Loop_loop422:
    for {
        var t423 int
        var inline490 int = ref_get__Ref_3int(j__3)
        t423 = inline490
        var t424 int = t423 + 1
        ref_set__Ref_3int(j__3, t424)
        var mtmp413 int
        var inline486 int = ref_get__Ref_3int(j__3)
        mtmp413 = inline486
        var jp426 int
        switch mtmp413 {
        case 5:
            break Loop_loop422
        default:
            var inline480 int = ref_get__Ref_3int(j__3)
            jp426 = inline480
            var t427 int
            var inline484 int = ref_get__Ref_3int(total__4)
            t427 = inline484
            var t428 int = t427 + jp426
            ref_set__Ref_3int(total__4, t428)
            continue
        }
    }
    var t421 int
    var inline495 int = ref_get__Ref_3int(total__4)
    t421 = inline495
    var inline492 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t421)
    _goml_runtime_core_string_println(inline492)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t458 string = _goml_runtime_core_int_to_string(self__151)
    return t458
}

func main() {
    main0()
}
