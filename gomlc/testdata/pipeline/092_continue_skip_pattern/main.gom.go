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
    var inline479 int = 0
    var inline480 *ref_int_x = ref__Ref_3int(inline479)
    i__0 = inline480
    Loop_loop416:
    for {
        var t417 int
        var inline473 int = ref_get__Ref_3int(i__0)
        t417 = inline473
        var t418 bool = t417 < 8
        if t418 {
            var t419 int
            var inline471 int = ref_get__Ref_3int(i__0)
            t419 = inline471
            var t420 int = t419 + 1
            ref_set__Ref_3int(i__0, t420)
            var t426 int
            var inline467 int = ref_get__Ref_3int(i__0)
            t426 = inline467
            var t427 bool
            var inline464 int = 3
            var inline465 bool = t426 == inline464
            t427 = inline465
            if t427 {
                continue
            } else {
                var t424 int
                var inline462 int = ref_get__Ref_3int(i__0)
                t424 = inline462
                var t425 bool
                var inline459 int = 6
                var inline460 bool = t424 == inline459
                t425 = inline460
                if t425 {
                    continue
                } else {
                    var t423 int
                    var inline457 int = ref_get__Ref_3int(i__0)
                    t423 = inline457
                    var inline454 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t423)
                    _goml_runtime_core_string_println(inline454)
                    continue
                }
            }
        } else {
            break Loop_loop416
        }
    }
    var inline475 string = "done"
    var inline476 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline475)
    _goml_runtime_core_string_println(inline476)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t447 string = _goml_runtime_core_int_to_string(self__151)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
