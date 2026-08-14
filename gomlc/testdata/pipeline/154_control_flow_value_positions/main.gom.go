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
    var inline508 int = 0
    var inline509 *ref_int_x = ref__Ref_3int(inline508)
    i__0 = inline509
    var sum__1 *ref_int_x
    var inline505 int = 0
    var inline506 *ref_int_x = ref__Ref_3int(inline505)
    sum__1 = inline506
    Loop_loop430:
    for {
        var t431 int
        var inline475 int = ref_get__Ref_3int(i__0)
        t431 = inline475
        var t432 bool = t431 < 5
        if t432 {
            var t433 int
            var inline473 int = ref_get__Ref_3int(i__0)
            t433 = inline473
            var t434 int = t433 + 1
            ref_set__Ref_3int(i__0, t434)
            var t439 int
            var inline469 int = ref_get__Ref_3int(i__0)
            t439 = inline469
            var t440 bool = t439 == 3
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
    var inline503 int = ref_get__Ref_3int(sum__1)
    t419 = inline503
    var inline500 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t419)
    _goml_runtime_core_string_println(inline500)
    var j__3 *ref_int_x
    var inline497 int = 0
    var inline498 *ref_int_x = ref__Ref_3int(inline497)
    j__3 = inline498
    var total__4 *ref_int_x
    var inline494 int = 0
    var inline495 *ref_int_x = ref__Ref_3int(inline494)
    total__4 = inline495
    Loop_loop422:
    for {
        var t423 int
        var inline487 int = ref_get__Ref_3int(j__3)
        t423 = inline487
        var t424 int = t423 + 1
        ref_set__Ref_3int(j__3, t424)
        var mtmp413 int
        var inline483 int = ref_get__Ref_3int(j__3)
        mtmp413 = inline483
        var jp426 int
        switch mtmp413 {
        case 5:
            break Loop_loop422
        default:
            var inline477 int = ref_get__Ref_3int(j__3)
            jp426 = inline477
            var t427 int
            var inline481 int = ref_get__Ref_3int(total__4)
            t427 = inline481
            var t428 int = t427 + jp426
            ref_set__Ref_3int(total__4, t428)
            continue
        }
    }
    var t421 int
    var inline492 int = ref_get__Ref_3int(total__4)
    t421 = inline492
    var inline489 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t421)
    _goml_runtime_core_string_println(inline489)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t455 string = _goml_runtime_core_int_to_string(self__151)
    return t455
}

func main() {
    main0()
}
