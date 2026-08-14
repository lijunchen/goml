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
    var inline467 int = 0
    var inline468 *ref_int_x = ref__Ref_3int(inline467)
    i__0 = inline468
    Loop_loop415:
    for {
        var t416 int
        var inline461 int = ref_get__Ref_3int(i__0)
        t416 = inline461
        var t417 bool = t416 < 10
        if t417 {
            var t418 int
            var inline459 int = ref_get__Ref_3int(i__0)
            t418 = inline459
            var t419 int = t418 + 1
            ref_set__Ref_3int(i__0, t419)
            var t422 int
            var inline455 int = ref_get__Ref_3int(i__0)
            t422 = inline455
            var t423 bool = t422 == 5
            if t423 {
                continue
            } else {
                var t421 int
                var inline453 int = ref_get__Ref_3int(i__0)
                t421 = inline453
                var inline450 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t421)
                _goml_runtime_core_string_println(inline450)
                continue
            }
        } else {
            break Loop_loop415
        }
    }
    var inline463 string = "done"
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline463)
    _goml_runtime_core_string_println(inline464)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t440 string = _goml_runtime_core_int_to_string(self__151)
    return t440
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
