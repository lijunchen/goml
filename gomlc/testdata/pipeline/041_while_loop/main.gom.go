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
    var inline501 int32 = 0
    var inline502 *ref_int32_x = ref__Ref_5int32(inline501)
    acc__1 = inline502
    var i__2 *ref_int32_x
    var inline498 int32 = 0
    var inline499 *ref_int32_x = ref__Ref_5int32(inline498)
    i__2 = inline499
    Loop_loop425:
    for {
        var t426 int32
        var inline494 int32 = ref_get__Ref_5int32(i__2)
        t426 = inline494
        var t427 bool = t426 < limit__0
        if t427 {
            var current__3 int32
            var inline492 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline492
            var t428 int32
            var inline490 int32 = ref_get__Ref_5int32(acc__1)
            t428 = inline490
            var t429 int32 = t428 + current__3
            ref_set__Ref_5int32(acc__1, t429)
            var t430 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t430)
            continue
        } else {
            break Loop_loop425
        }
    }
    var inline496 int32 = ref_get__Ref_5int32(acc__1)
    return inline496
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline526 int32 = 0
    var inline527 *ref_int32_x = ref__Ref_5int32(inline526)
    acc__5 = inline527
    var i__6 *ref_int32_x
    var inline523 int32 = 0
    var inline524 *ref_int32_x = ref__Ref_5int32(inline523)
    i__6 = inline524
    var is_even__7 *ref_bool_x
    var inline520 bool = true
    var inline521 *ref_bool_x = ref__Ref_4bool(inline520)
    is_even__7 = inline521
    Loop_loop435:
    for {
        var t436 int32
        var inline516 int32 = ref_get__Ref_5int32(i__6)
        t436 = inline516
        var t437 bool = t436 < limit__4
        if t437 {
            var current__8 int32
            var inline514 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline514
            var t438 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t438)
            var add_now__9 bool
            var inline510 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline510
            var t439 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t439)
            if add_now__9 {
                var t441 int32
                var inline506 int32 = ref_get__Ref_5int32(acc__5)
                t441 = inline506
                var t442 int32 = t441 + current__8
                ref_set__Ref_5int32(acc__5, t442)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop435
        }
    }
    var inline518 int32 = ref_get__Ref_5int32(acc__5)
    return inline518
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline539 string = "sum_to(5)="
    var inline540 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline539)
    _goml_runtime_core_string_print(inline540)
    var inline536 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline536)
    var inline532 string = "sum_even(6)="
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline532)
    _goml_runtime_core_string_print(inline533)
    var inline529 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline529)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t471 string = _goml_runtime_core_int32_to_string(self__154)
    return t471
}

func main() {
    main0()
}
