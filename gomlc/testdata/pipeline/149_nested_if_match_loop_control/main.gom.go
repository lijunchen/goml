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
    var inline234 int = 0
    var inline235 *ref_int_x = ref__Ref_3int(inline234)
    i__0 = inline235
    var sum__1 *ref_int_x
    var inline231 int = 0
    var inline232 *ref_int_x = ref__Ref_3int(inline231)
    sum__1 = inline232
    Loop_loop185:
    for {
        var t186 int
        var inline224 int = ref_get__Ref_3int(i__0)
        t186 = inline224
        var t187 bool = t186 < 7
        if t187 {
            var cur__2 int
            var inline222 int = ref_get__Ref_3int(i__0)
            cur__2 = inline222
            var t188 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t188)
            var t192 bool = cur__2 < 5
            if t192 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t190 int
                    var inline218 int = ref_get__Ref_3int(sum__1)
                    t190 = inline218
                    var t191 int = t190 + cur__2
                    ref_set__Ref_3int(sum__1, t191)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop185
                default:
                    var t190 int
                    var inline218 int = ref_get__Ref_3int(sum__1)
                    t190 = inline218
                    var t191 int = t190 + cur__2
                    ref_set__Ref_3int(sum__1, t191)
                    continue
                }
            }
        } else {
            break Loop_loop185
        }
    }
    var t184 int
    var inline229 int = ref_get__Ref_3int(sum__1)
    t184 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t184)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t208 string = _goml_runtime_core_int_to_string(self__69)
    return t208
}

func main() {
    main0()
}
