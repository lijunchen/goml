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
            var t169 int
            var inline209 int = ref_get__Ref_3int(i__0)
            t169 = inline209
            var t170 bool
            var inline206 int = 5
            var inline207 bool = t169 == inline206
            t170 = inline207
            if t170 {
                break Loop_loop162
            } else {
                var t166 int
                var inline204 int = ref_get__Ref_3int(i__0)
                t166 = inline204
                var inline201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t166)
                _goml_runtime_core_string_println(inline201)
                var t167 int
                var inline199 int = ref_get__Ref_3int(i__0)
                t167 = inline199
                var t168 int = t167 + 1
                ref_set__Ref_3int(i__0, t168)
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
