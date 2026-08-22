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

type Option__bool struct {
    _tag int32
    _v1_0 bool
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func run_some() Option__i32 {
    var i__2 *ref_int32_x
    var inline520 int32 = 0
    var inline521 *ref_int32_x = ref__Ref_5int32(inline520)
    i__2 = inline521
    var total__3 *ref_int32_x
    var inline517 int32 = 0
    var inline518 *ref_int32_x = ref__Ref_5int32(inline517)
    total__3 = inline518
    Loop_loop442:
    for {
        var t443 int32
        var inline513 int32 = ref_get__Ref_5int32(i__2)
        t443 = inline513
        var mtmp411 Option__bool
        var inline509 bool = t443 < 3
        if inline509 {
            var inline510 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp411 = inline510
        } else {
            var inline511 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: false,
            }
            mtmp411 = inline511
        }
        var jp445 bool
        switch mtmp411._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x412 bool = mtmp411._v1_0
            jp445 = x412
            if jp445 {
                var t446 int32
                var inline507 int32 = ref_get__Ref_5int32(total__3)
                t446 = inline507
                var t447 int32
                var inline505 int32 = ref_get__Ref_5int32(i__2)
                t447 = inline505
                var t448 int32 = t446 + t447
                ref_set__Ref_5int32(total__3, t448)
                var t449 int32
                var inline501 int32 = ref_get__Ref_5int32(i__2)
                t449 = inline501
                var t450 int32 = t449 + 1
                ref_set__Ref_5int32(i__2, t450)
                continue
            } else {
                break Loop_loop442
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t440 int32
    var inline515 int32 = ref_get__Ref_5int32(total__3)
    t440 = inline515
    var t441 Option__i32 = Option__i32{
        _tag: 1,
        _v1_0: t440,
    }
    return t441
}

func run_none() Option__i32 {
    var i__4 *ref_int32_x
    var inline543 int32 = 0
    var inline544 *ref_int32_x = ref__Ref_5int32(inline543)
    i__4 = inline544
    var total__5 *ref_int32_x
    var inline540 int32 = 0
    var inline541 *ref_int32_x = ref__Ref_5int32(inline540)
    total__5 = inline541
    Loop_loop456:
    for {
        var t457 int32
        var inline536 int32 = ref_get__Ref_5int32(i__4)
        t457 = inline536
        var mtmp416 Option__bool
        var inline533 bool = t457 < 2
        if inline533 {
            var inline534 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp416 = inline534
        } else {
            mtmp416 = Option__bool{
                _tag: 0,
            }
        }
        var jp459 bool
        switch mtmp416._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x417 bool = mtmp416._v1_0
            jp459 = x417
            if jp459 {
                var t460 int32
                var inline531 int32 = ref_get__Ref_5int32(total__5)
                t460 = inline531
                var t461 int32
                var inline529 int32 = ref_get__Ref_5int32(i__4)
                t461 = inline529
                var t462 int32 = t460 + t461
                ref_set__Ref_5int32(total__5, t462)
                var t463 int32
                var inline525 int32 = ref_get__Ref_5int32(i__4)
                t463 = inline525
                var t464 int32 = t463 + 1
                ref_set__Ref_5int32(i__4, t464)
                continue
            } else {
                break Loop_loop456
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t454 int32
    var inline538 int32 = ref_get__Ref_5int32(total__5)
    t454 = inline538
    var t455 Option__i32 = Option__i32{
        _tag: 1,
        _v1_0: t454,
    }
    return t455
}

func main0() struct{} {
    var t472 Option__i32 = run_some()
    var t473 string
    switch t472._tag {
    case 0:
        t473 = "none"
    case 1:
        var inline559 int32 = t472._v1_0
        var inline561 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline559)
        var inline562 string = "some=" + inline561
        t473 = inline562
    default:
        panic("non-exhaustive match")
    }
    var inline556 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t473)
    _goml_runtime_core_string_println(inline556)
    var t474 Option__i32 = run_none()
    var t475 string
    switch t474._tag {
    case 0:
        t475 = "none"
    case 1:
        var inline551 int32 = t474._v1_0
        var inline553 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline551)
        var inline554 string = "some=" + inline553
        t475 = inline554
    default:
        panic("non-exhaustive match")
    }
    var inline548 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
    _goml_runtime_core_string_println(inline548)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t486 string = _goml_runtime_core_int32_to_string(self__33)
    return t486
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
