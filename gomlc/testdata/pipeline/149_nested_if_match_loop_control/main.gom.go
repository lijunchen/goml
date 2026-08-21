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
    var inline468 int = 0
    var inline469 *ref_int_x = ref__Ref_3int(inline468)
    i__0 = inline469
    var sum__1 *ref_int_x
    var inline465 int = 0
    var inline466 *ref_int_x = ref__Ref_3int(inline465)
    sum__1 = inline466
    Loop_loop419:
    for {
        var t420 int
        var inline458 int = ref_get__Ref_3int(i__0)
        t420 = inline458
        var t421 bool = t420 < 7
        if t421 {
            var cur__2 int
            var inline456 int = ref_get__Ref_3int(i__0)
            cur__2 = inline456
            var t422 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t422)
            var t426 bool = cur__2 < 5
            if t426 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t424 int
                    var inline452 int = ref_get__Ref_3int(sum__1)
                    t424 = inline452
                    var t425 int = t424 + cur__2
                    ref_set__Ref_3int(sum__1, t425)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop419
                default:
                    var t424 int
                    var inline452 int = ref_get__Ref_3int(sum__1)
                    t424 = inline452
                    var t425 int = t424 + cur__2
                    ref_set__Ref_3int(sum__1, t425)
                    continue
                }
            }
        } else {
            break Loop_loop419
        }
    }
    var t418 int
    var inline463 int = ref_get__Ref_3int(sum__1)
    t418 = inline463
    var inline460 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t418)
    _goml_runtime_core_string_println(inline460)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t442 string = _goml_runtime_core_int_to_string(self__151)
    return t442
}

func main() {
    main0()
}
