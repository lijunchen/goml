package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

func sum_to(limit__0 int32) int32 {
    var acc__1 *ref_int32_x
    var inline498 int32 = 0
    var inline499 *ref_int32_x = ref__Ref_5int32(inline498)
    acc__1 = inline499
    var i__2 *ref_int32_x
    var inline495 int32 = 0
    var inline496 *ref_int32_x = ref__Ref_5int32(inline495)
    i__2 = inline496
    Loop_loop422:
    for {
        var t423 int32
        var inline491 int32 = ref_get__Ref_5int32(i__2)
        t423 = inline491
        var t424 bool = t423 < limit__0
        if t424 {
            var current__3 int32
            var inline489 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline489
            var t425 int32
            var inline487 int32 = ref_get__Ref_5int32(acc__1)
            t425 = inline487
            var t426 int32 = t425 + current__3
            ref_set__Ref_5int32(acc__1, t426)
            var t427 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t427)
            continue
        } else {
            break Loop_loop422
        }
    }
    var inline493 int32 = ref_get__Ref_5int32(acc__1)
    return inline493
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline523 int32 = 0
    var inline524 *ref_int32_x = ref__Ref_5int32(inline523)
    acc__5 = inline524
    var i__6 *ref_int32_x
    var inline520 int32 = 0
    var inline521 *ref_int32_x = ref__Ref_5int32(inline520)
    i__6 = inline521
    var is_even__7 *ref_bool_x
    var inline517 bool = true
    var inline518 *ref_bool_x = ref__Ref_4bool(inline517)
    is_even__7 = inline518
    Loop_loop432:
    for {
        var t433 int32
        var inline513 int32 = ref_get__Ref_5int32(i__6)
        t433 = inline513
        var t434 bool = t433 < limit__4
        if t434 {
            var current__8 int32
            var inline511 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline511
            var t435 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t435)
            var add_now__9 bool
            var inline507 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline507
            var t436 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t436)
            if add_now__9 {
                var t438 int32
                var inline503 int32 = ref_get__Ref_5int32(acc__5)
                t438 = inline503
                var t439 int32 = t438 + current__8
                ref_set__Ref_5int32(acc__5, t439)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop432
        }
    }
    var inline515 int32 = ref_get__Ref_5int32(acc__5)
    return inline515
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline536 string = "sum_to(5)="
    var inline537 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline536)
    _goml_runtime_core_string_print(inline537)
    var inline533 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline533)
    var inline529 string = "sum_even(6)="
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline529)
    _goml_runtime_core_string_print(inline530)
    var inline526 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline526)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t468 string = _goml_runtime_core_int32_to_string(self__154)
    return t468
}

func main() {
    main0()
}
