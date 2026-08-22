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
    var inline476 int = 0
    var inline477 *ref_int_x = ref__Ref_3int(inline476)
    i__0 = inline477
    Loop_loop419:
    for {
        var t420 int
        var inline470 int = ref_get__Ref_3int(i__0)
        t420 = inline470
        var t421 bool = t420 < 8
        if t421 {
            var t422 int
            var inline468 int = ref_get__Ref_3int(i__0)
            t422 = inline468
            var t423 int = t422 + 1
            ref_set__Ref_3int(i__0, t423)
            var t429 int
            var inline464 int = ref_get__Ref_3int(i__0)
            t429 = inline464
            var t430 bool = t429 == 3
            if t430 {
                continue
            } else {
                var t427 int
                var inline462 int = ref_get__Ref_3int(i__0)
                t427 = inline462
                var t428 bool = t427 == 6
                if t428 {
                    continue
                } else {
                    var t426 int
                    var inline460 int = ref_get__Ref_3int(i__0)
                    t426 = inline460
                    var inline457 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t426)
                    _goml_runtime_core_string_println(inline457)
                    continue
                }
            }
        } else {
            break Loop_loop419
        }
    }
    var inline472 string = "done"
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline472)
    _goml_runtime_core_string_println(inline473)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t447 string = _goml_runtime_core_int_to_string(self__151)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
