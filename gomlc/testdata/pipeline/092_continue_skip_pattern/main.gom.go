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
    var inline253 int = 0
    var inline254 *ref_int_x = ref__Ref_3int(inline253)
    i__0 = inline254
    Loop_loop190:
    for {
        var t191 int
        var inline247 int = ref_get__Ref_3int(i__0)
        t191 = inline247
        var t192 bool = t191 < 8
        if t192 {
            var t193 int
            var inline245 int = ref_get__Ref_3int(i__0)
            t193 = inline245
            var t194 int = t193 + 1
            ref_set__Ref_3int(i__0, t194)
            var t200 int
            var inline241 int = ref_get__Ref_3int(i__0)
            t200 = inline241
            var t201 bool
            var inline238 int = 3
            var inline239 bool = t200 == inline238
            t201 = inline239
            if t201 {
                continue
            } else {
                var t198 int
                var inline236 int = ref_get__Ref_3int(i__0)
                t198 = inline236
                var t199 bool
                var inline233 int = 6
                var inline234 bool = t198 == inline233
                t199 = inline234
                if t199 {
                    continue
                } else {
                    var t197 int
                    var inline231 int = ref_get__Ref_3int(i__0)
                    t197 = inline231
                    var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t197)
                    _goml_runtime_core_string_println(inline228)
                    continue
                }
            }
        } else {
            break Loop_loop190
        }
    }
    var inline249 string = "done"
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t221 string = _goml_runtime_core_int_to_string(self__67)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
