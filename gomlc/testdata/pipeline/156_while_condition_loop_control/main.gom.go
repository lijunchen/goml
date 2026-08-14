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
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop437:
    for {
        var t448 int
        var inline495 int = ref_get__Ref_3int(i__0)
        t448 = inline495
        var t449 bool = t448 == 0
        var jp439 bool
        if t449 {
            var inline475 int = 1
            ref_set__Ref_3int(i__0, inline475)
            jp439 = true
        } else {
            var t452 int
            var inline478 int = ref_get__Ref_3int(i__0)
            t452 = inline478
            var t453 bool = t452 < 4
            if t453 {
                jp439 = true
            } else {
                jp439 = false
            }
        }
        if jp439 {
            var t440 int
            var inline493 int = ref_get__Ref_3int(total__1)
            t440 = inline493
            var t441 int
            var inline491 int = ref_get__Ref_3int(i__0)
            t441 = inline491
            var t442 int = t440 + t441
            ref_set__Ref_3int(total__1, t442)
            var t446 int
            var inline487 int = ref_get__Ref_3int(i__0)
            t446 = inline487
            var t447 bool = t446 == 1
            if t447 {
                var inline480 int = 2
                ref_set__Ref_3int(i__0, inline480)
                continue
            } else {
                var t444 int
                var inline485 int = ref_get__Ref_3int(i__0)
                t444 = inline485
                var t445 int = t444 + 1
                ref_set__Ref_3int(i__0, t445)
                continue
            }
        } else {
            break Loop_loop437
        }
    }
    var t425 int
    var inline527 int = ref_get__Ref_3int(total__1)
    t425 = inline527
    var inline524 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t425)
    _goml_runtime_core_string_println(inline524)
    var j__2 *ref_int_x
    var inline521 int = 0
    var inline522 *ref_int_x = ref__Ref_3int(inline521)
    j__2 = inline522
    var total2__3 *ref_int_x
    var inline518 int = 0
    var inline519 *ref_int_x = ref__Ref_3int(inline518)
    total2__3 = inline519
    Loop_loop428:
    for {
        var mtmp415 int
        var inline511 int = ref_get__Ref_3int(j__2)
        mtmp415 = inline511
        var jp430 bool
        switch mtmp415 {
        case 0:
            var inline497 int = 1
            ref_set__Ref_3int(j__2, inline497)
            jp430 = true
        case 1:
            var inline500 int = 2
            ref_set__Ref_3int(j__2, inline500)
            jp430 = true
        case 2:
            jp430 = true
        default:
            jp430 = false
        }
        if jp430 {
            var t431 int
            var inline509 int = ref_get__Ref_3int(total2__3)
            t431 = inline509
            var t432 int
            var inline507 int = ref_get__Ref_3int(j__2)
            t432 = inline507
            var t433 int = t431 + t432
            ref_set__Ref_3int(total2__3, t433)
            var t435 int
            var inline503 int = ref_get__Ref_3int(j__2)
            t435 = inline503
            var t436 bool = t435 == 2
            if t436 {
                break Loop_loop428
            } else {
                continue
            }
        } else {
            break Loop_loop428
        }
    }
    var t427 int
    var inline516 int = ref_get__Ref_3int(total2__3)
    t427 = inline516
    var inline513 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t427)
    _goml_runtime_core_string_println(inline513)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t456 *ref_int_x = ref__Ref_3int(value__431)
    return t456
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t467 string = _goml_runtime_core_int_to_string(self__151)
    return t467
}

func main() {
    main0()
}
