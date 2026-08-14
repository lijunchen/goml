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
    var inline249 int = 0
    var inline250 *ref_int_x = ref__Ref_3int(inline249)
    i__0 = inline250
    Loop_loop194:
    for {
        var t195 int
        var inline243 int = ref_get__Ref_3int(i__0)
        t195 = inline243
        var t196 bool = t195 < 10
        if t196 {
            var t201 int
            var inline241 int = ref_get__Ref_3int(i__0)
            t201 = inline241
            var t202 bool
            var inline238 int = 5
            var inline239 bool = t201 == inline238
            t202 = inline239
            if t202 {
                break Loop_loop194
            } else {
                var t198 int
                var inline236 int = ref_get__Ref_3int(i__0)
                t198 = inline236
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
                _goml_runtime_core_string_println(inline233)
                var t199 int
                var inline231 int = ref_get__Ref_3int(i__0)
                t199 = inline231
                var t200 int = t199 + 1
                ref_set__Ref_3int(i__0, t200)
                continue
            }
        } else {
            break Loop_loop194
        }
    }
    var inline245 string = "done"
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t222 string = _goml_runtime_core_int_to_string(self__67)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
