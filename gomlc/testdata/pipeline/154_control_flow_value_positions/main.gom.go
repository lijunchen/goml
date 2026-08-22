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
    Loop_loop433:
    for {
        var t434 int
        var inline478 int = ref_get__Ref_3int(i__0)
        t434 = inline478
        var t435 bool = t434 < 5
        if t435 {
            var t436 int
            var inline476 int = ref_get__Ref_3int(i__0)
            t436 = inline476
            var t437 int = t436 + 1
            ref_set__Ref_3int(i__0, t437)
            var t442 int
            var inline472 int = ref_get__Ref_3int(i__0)
            t442 = inline472
            var t443 bool = t442 == 3
            var jp439 int
            if t443 {
                continue
            } else {
                var inline466 int = ref_get__Ref_3int(i__0)
                jp439 = inline466
                var t440 int
                var inline470 int = ref_get__Ref_3int(sum__1)
                t440 = inline470
                var t441 int = t440 + jp439
                ref_set__Ref_3int(sum__1, t441)
                continue
            }
        } else {
            break Loop_loop433
        }
    }
    var t422 int
    var inline506 int = ref_get__Ref_3int(sum__1)
    t422 = inline506
    var inline503 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t422)
    _goml_runtime_core_string_println(inline503)
    var j__3 *ref_int_x
    var inline500 int = 0
    var inline501 *ref_int_x = ref__Ref_3int(inline500)
    j__3 = inline501
    var total__4 *ref_int_x
    var inline497 int = 0
    var inline498 *ref_int_x = ref__Ref_3int(inline497)
    total__4 = inline498
    Loop_loop425:
    for {
        var t426 int
        var inline490 int = ref_get__Ref_3int(j__3)
        t426 = inline490
        var t427 int = t426 + 1
        ref_set__Ref_3int(j__3, t427)
        var mtmp416 int
        var inline486 int = ref_get__Ref_3int(j__3)
        mtmp416 = inline486
        var jp429 int
        switch mtmp416 {
        case 5:
            break Loop_loop425
        default:
            var inline480 int = ref_get__Ref_3int(j__3)
            jp429 = inline480
            var t430 int
            var inline484 int = ref_get__Ref_3int(total__4)
            t430 = inline484
            var t431 int = t430 + jp429
            ref_set__Ref_3int(total__4, t431)
            continue
        }
    }
    var t424 int
    var inline495 int = ref_get__Ref_3int(total__4)
    t424 = inline495
    var inline492 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t424)
    _goml_runtime_core_string_println(inline492)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t458 string = _goml_runtime_core_int_to_string(self__151)
    return t458
}

func main() {
    main0()
}
