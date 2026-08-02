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
    var inline217 int = 0
    var inline218 *ref_int_x = ref__Ref_3int(inline217)
    i__0 = inline218
    Loop_loop162:
    for {
        var t163 int
        var inline211 int = ref_get__Ref_3int(i__0)
        t163 = inline211
        var t164 bool = t163 < 10
        if t164 {
            var t165 int
            var inline209 int = ref_get__Ref_3int(i__0)
            t165 = inline209
            var t166 int = t165 + 1
            ref_set__Ref_3int(i__0, t166)
            var t169 int
            var inline205 int = ref_get__Ref_3int(i__0)
            t169 = inline205
            var t170 bool
            var inline202 int = 5
            var inline203 bool = t169 == inline202
            t170 = inline203
            if t170 {
                continue
            } else {
                var t168 int
                var inline200 int = ref_get__Ref_3int(i__0)
                t168 = inline200
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t168)
                _goml_runtime_core_string_println(inline197)
                continue
            }
        } else {
            break Loop_loop162
        }
    }
    var inline213 string = "done"
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline213)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t190 string = _goml_runtime_core_int_to_string(self__40)
    return t190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
