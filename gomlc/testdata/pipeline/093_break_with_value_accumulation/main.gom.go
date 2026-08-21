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
    var inline495 int = 0
    var inline496 *ref_int_x = ref__Ref_3int(inline495)
    sum__0 = inline496
    var i__1 *ref_int_x
    var inline492 int = 0
    var inline493 *ref_int_x = ref__Ref_3int(inline492)
    i__1 = inline493
    Loop_loop423:
    for {
        var t424 int
        var inline472 int = ref_get__Ref_3int(i__1)
        t424 = inline472
        var t425 bool = t424 < 20
        if t425 {
            var t426 int
            var inline470 int = ref_get__Ref_3int(i__1)
            t426 = inline470
            var t427 int = t426 + 1
            ref_set__Ref_3int(i__1, t427)
            var t432 int
            var inline466 int = ref_get__Ref_3int(i__1)
            t432 = inline466
            var t433 bool = t432 > 5
            if t433 {
                break Loop_loop423
            } else {
                var t429 int
                var inline464 int = ref_get__Ref_3int(sum__0)
                t429 = inline464
                var t430 int
                var inline462 int = ref_get__Ref_3int(i__1)
                t430 = inline462
                var t431 int = t429 + t430
                ref_set__Ref_3int(sum__0, t431)
                continue
            }
        } else {
            break Loop_loop423
        }
    }
    var inline488 string = "sum: "
    var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline488)
    _goml_runtime_core_string_print(inline489)
    var t421 int
    var inline486 int = ref_get__Ref_3int(sum__0)
    t421 = inline486
    var inline483 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t421)
    _goml_runtime_core_string_println(inline483)
    var inline479 string = "i at break: "
    var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline479)
    _goml_runtime_core_string_print(inline480)
    var t422 int
    var inline477 int = ref_get__Ref_3int(i__1)
    t422 = inline477
    var inline474 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t422)
    _goml_runtime_core_string_println(inline474)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t452 string = _goml_runtime_core_int_to_string(self__151)
    return t452
}

func main() {
    main0()
}
