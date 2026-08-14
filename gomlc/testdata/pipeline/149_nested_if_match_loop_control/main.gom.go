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

func main0() struct{} {
    var i__0 *ref_int_x
    var inline244 int = 0
    var inline245 *ref_int_x = ref__Ref_3int(inline244)
    i__0 = inline245
    var sum__1 *ref_int_x
    var inline241 int = 0
    var inline242 *ref_int_x = ref__Ref_3int(inline241)
    sum__1 = inline242
    Loop_loop195:
    for {
        var t196 int
        var inline234 int = ref_get__Ref_3int(i__0)
        t196 = inline234
        var t197 bool = t196 < 7
        if t197 {
            var cur__2 int
            var inline232 int = ref_get__Ref_3int(i__0)
            cur__2 = inline232
            var t198 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t198)
            var t202 bool = cur__2 < 5
            if t202 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t200 int
                    var inline228 int = ref_get__Ref_3int(sum__1)
                    t200 = inline228
                    var t201 int = t200 + cur__2
                    ref_set__Ref_3int(sum__1, t201)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop195
                default:
                    var t200 int
                    var inline228 int = ref_get__Ref_3int(sum__1)
                    t200 = inline228
                    var t201 int = t200 + cur__2
                    ref_set__Ref_3int(sum__1, t201)
                    continue
                }
            }
        } else {
            break Loop_loop195
        }
    }
    var t194 int
    var inline239 int = ref_get__Ref_3int(sum__1)
    t194 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t194)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t218 string = _goml_runtime_core_int_to_string(self__67)
    return t218
}

func main() {
    main0()
}
