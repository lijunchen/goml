package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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
    var sum__0 *ref_int_x
    var inline526 int = 0
    var inline527 *ref_int_x = ref__Ref_3int(inline526)
    sum__0 = inline527
    var i__1 *ref_int_x
    var inline523 int = 1
    var inline524 *ref_int_x = ref__Ref_3int(inline523)
    i__1 = inline524
    Loop_loop438:
    for {
        var t439 int
        var inline487 int = ref_get__Ref_3int(i__1)
        t439 = inline487
        var t440 bool = t439 <= 100
        if t440 {
            var t447 int
            var inline485 int = ref_get__Ref_3int(i__1)
            t447 = inline485
            var t448 bool = t447 == 50
            if t448 {
                break Loop_loop438
            } else {
                var t442 int
                var inline483 int = ref_get__Ref_3int(sum__0)
                t442 = inline483
                var t443 int
                var inline481 int = ref_get__Ref_3int(i__1)
                t443 = inline481
                var t444 int = t442 + t443
                ref_set__Ref_3int(sum__0, t444)
                var t445 int
                var inline477 int = ref_get__Ref_3int(i__1)
                t445 = inline477
                var t446 int = t445 + 1
                ref_set__Ref_3int(i__1, t446)
                continue
            }
        } else {
            break Loop_loop438
        }
    }
    var inline519 string = "sum up to break: "
    var inline520 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline519)
    _goml_runtime_core_string_print(inline520)
    var t425 int
    var inline517 int = ref_get__Ref_3int(sum__0)
    t425 = inline517
    var inline514 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t425)
    _goml_runtime_core_string_println(inline514)
    var even_sum__2 *ref_int_x
    var inline511 int = 0
    var inline512 *ref_int_x = ref__Ref_3int(inline511)
    even_sum__2 = inline512
    var j__3 *ref_int_x
    var inline508 int = 1
    var inline509 *ref_int_x = ref__Ref_3int(inline508)
    j__3 = inline509
    Loop_loop428:
    for {
        var t429 int
        var inline497 int = ref_get__Ref_3int(j__3)
        t429 = inline497
        var t430 bool = t429 <= 10
        if t430 {
            var cur__4 int
            var inline495 int = ref_get__Ref_3int(j__3)
            cur__4 = inline495
            var t431 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t431)
            var t433 int = cur__4 / 2
            var t434 int = t433 * 2
            var t435 bool = cur__4 == t434
            if t435 {
                var t436 int
                var inline491 int = ref_get__Ref_3int(even_sum__2)
                t436 = inline491
                var t437 int = t436 + cur__4
                ref_set__Ref_3int(even_sum__2, t437)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop428
        }
    }
    var inline504 string = "even sum: "
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline504)
    _goml_runtime_core_string_print(inline505)
    var t427 int
    var inline502 int = ref_get__Ref_3int(even_sum__2)
    t427 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t427)
    _goml_runtime_core_string_println(inline499)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t467 string = _goml_runtime_core_int_to_string(self__151)
    return t467
}

func main() {
    main0()
}
