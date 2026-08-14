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
    var inline465 int = 0
    var inline466 *ref_int_x = ref__Ref_3int(inline465)
    i__0 = inline466
    var sum__1 *ref_int_x
    var inline462 int = 0
    var inline463 *ref_int_x = ref__Ref_3int(inline462)
    sum__1 = inline463
    Loop_loop416:
    for {
        var t417 int
        var inline455 int = ref_get__Ref_3int(i__0)
        t417 = inline455
        var t418 bool = t417 < 7
        if t418 {
            var cur__2 int
            var inline453 int = ref_get__Ref_3int(i__0)
            cur__2 = inline453
            var t419 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t419)
            var t423 bool = cur__2 < 5
            if t423 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t421 int
                    var inline449 int = ref_get__Ref_3int(sum__1)
                    t421 = inline449
                    var t422 int = t421 + cur__2
                    ref_set__Ref_3int(sum__1, t422)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop416
                default:
                    var t421 int
                    var inline449 int = ref_get__Ref_3int(sum__1)
                    t421 = inline449
                    var t422 int = t421 + cur__2
                    ref_set__Ref_3int(sum__1, t422)
                    continue
                }
            }
        } else {
            break Loop_loop416
        }
    }
    var t415 int
    var inline460 int = ref_get__Ref_3int(sum__1)
    t415 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t415)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t439 string = _goml_runtime_core_int_to_string(self__151)
    return t439
}

func main() {
    main0()
}
