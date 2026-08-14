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
    var inline523 int = 0
    var inline524 *ref_int_x = ref__Ref_3int(inline523)
    sum__0 = inline524
    var i__1 *ref_int_x
    var inline520 int = 1
    var inline521 *ref_int_x = ref__Ref_3int(inline520)
    i__1 = inline521
    Loop_loop435:
    for {
        var t436 int
        var inline484 int = ref_get__Ref_3int(i__1)
        t436 = inline484
        var t437 bool = t436 <= 100
        if t437 {
            var t444 int
            var inline482 int = ref_get__Ref_3int(i__1)
            t444 = inline482
            var t445 bool = t444 == 50
            if t445 {
                break Loop_loop435
            } else {
                var t439 int
                var inline480 int = ref_get__Ref_3int(sum__0)
                t439 = inline480
                var t440 int
                var inline478 int = ref_get__Ref_3int(i__1)
                t440 = inline478
                var t441 int = t439 + t440
                ref_set__Ref_3int(sum__0, t441)
                var t442 int
                var inline474 int = ref_get__Ref_3int(i__1)
                t442 = inline474
                var t443 int = t442 + 1
                ref_set__Ref_3int(i__1, t443)
                continue
            }
        } else {
            break Loop_loop435
        }
    }
    var inline516 string = "sum up to break: "
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline516)
    _goml_runtime_core_string_print(inline517)
    var t422 int
    var inline514 int = ref_get__Ref_3int(sum__0)
    t422 = inline514
    var inline511 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t422)
    _goml_runtime_core_string_println(inline511)
    var even_sum__2 *ref_int_x
    var inline508 int = 0
    var inline509 *ref_int_x = ref__Ref_3int(inline508)
    even_sum__2 = inline509
    var j__3 *ref_int_x
    var inline505 int = 1
    var inline506 *ref_int_x = ref__Ref_3int(inline505)
    j__3 = inline506
    Loop_loop425:
    for {
        var t426 int
        var inline494 int = ref_get__Ref_3int(j__3)
        t426 = inline494
        var t427 bool = t426 <= 10
        if t427 {
            var cur__4 int
            var inline492 int = ref_get__Ref_3int(j__3)
            cur__4 = inline492
            var t428 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t428)
            var t430 int = cur__4 / 2
            var t431 int = t430 * 2
            var t432 bool = cur__4 == t431
            if t432 {
                var t433 int
                var inline488 int = ref_get__Ref_3int(even_sum__2)
                t433 = inline488
                var t434 int = t433 + cur__4
                ref_set__Ref_3int(even_sum__2, t434)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop425
        }
    }
    var inline501 string = "even sum: "
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline501)
    _goml_runtime_core_string_print(inline502)
    var t424 int
    var inline499 int = ref_get__Ref_3int(even_sum__2)
    t424 = inline499
    var inline496 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t424)
    _goml_runtime_core_string_println(inline496)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t464 string = _goml_runtime_core_int_to_string(self__151)
    return t464
}

func main() {
    main0()
}
