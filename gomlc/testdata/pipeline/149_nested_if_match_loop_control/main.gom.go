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
    var inline239 int = 0
    var inline240 *ref_int_x = ref__Ref_3int(inline239)
    i__0 = inline240
    var sum__1 *ref_int_x
    var inline236 int = 0
    var inline237 *ref_int_x = ref__Ref_3int(inline236)
    sum__1 = inline237
    Loop_loop190:
    for {
        var t191 int
        var inline229 int = ref_get__Ref_3int(i__0)
        t191 = inline229
        var t192 bool = t191 < 7
        if t192 {
            var cur__2 int
            var inline227 int = ref_get__Ref_3int(i__0)
            cur__2 = inline227
            var t193 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t193)
            var t197 bool = cur__2 < 5
            if t197 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t195 int
                    var inline223 int = ref_get__Ref_3int(sum__1)
                    t195 = inline223
                    var t196 int = t195 + cur__2
                    ref_set__Ref_3int(sum__1, t196)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop190
                default:
                    var t195 int
                    var inline223 int = ref_get__Ref_3int(sum__1)
                    t195 = inline223
                    var t196 int = t195 + cur__2
                    ref_set__Ref_3int(sum__1, t196)
                    continue
                }
            }
        } else {
            break Loop_loop190
        }
    }
    var t189 int
    var inline234 int = ref_get__Ref_3int(sum__1)
    t189 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t213 string = _goml_runtime_core_int_to_string(self__67)
    return t213
}

func main() {
    main0()
}
