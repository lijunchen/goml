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
    var inline492 int = 0
    var inline493 *ref_int_x = ref__Ref_3int(inline492)
    sum__0 = inline493
    var i__1 *ref_int_x
    var inline489 int = 0
    var inline490 *ref_int_x = ref__Ref_3int(inline489)
    i__1 = inline490
    Loop_loop420:
    for {
        var t421 int
        var inline469 int = ref_get__Ref_3int(i__1)
        t421 = inline469
        var t422 bool = t421 < 20
        if t422 {
            var t423 int
            var inline467 int = ref_get__Ref_3int(i__1)
            t423 = inline467
            var t424 int = t423 + 1
            ref_set__Ref_3int(i__1, t424)
            var t429 int
            var inline463 int = ref_get__Ref_3int(i__1)
            t429 = inline463
            var t430 bool = t429 > 5
            if t430 {
                break Loop_loop420
            } else {
                var t426 int
                var inline461 int = ref_get__Ref_3int(sum__0)
                t426 = inline461
                var t427 int
                var inline459 int = ref_get__Ref_3int(i__1)
                t427 = inline459
                var t428 int = t426 + t427
                ref_set__Ref_3int(sum__0, t428)
                continue
            }
        } else {
            break Loop_loop420
        }
    }
    var inline485 string = "sum: "
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline485)
    _goml_runtime_core_string_print(inline486)
    var t418 int
    var inline483 int = ref_get__Ref_3int(sum__0)
    t418 = inline483
    var inline480 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t418)
    _goml_runtime_core_string_println(inline480)
    var inline476 string = "i at break: "
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline476)
    _goml_runtime_core_string_print(inline477)
    var t419 int
    var inline474 int = ref_get__Ref_3int(i__1)
    t419 = inline474
    var inline471 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t419)
    _goml_runtime_core_string_println(inline471)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t449 string = _goml_runtime_core_int_to_string(self__151)
    return t449
}

func main() {
    main0()
}
