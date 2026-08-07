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
    var inline243 int = 0
    var inline244 *ref_int_x = ref__Ref_3int(inline243)
    i__0 = inline244
    Loop_loop180:
    for {
        var t181 int
        var inline237 int = ref_get__Ref_3int(i__0)
        t181 = inline237
        var t182 bool = t181 < 8
        if t182 {
            var t183 int
            var inline235 int = ref_get__Ref_3int(i__0)
            t183 = inline235
            var t184 int = t183 + 1
            ref_set__Ref_3int(i__0, t184)
            var t190 int
            var inline231 int = ref_get__Ref_3int(i__0)
            t190 = inline231
            var t191 bool
            var inline228 int = 3
            var inline229 bool = t190 == inline228
            t191 = inline229
            if t191 {
                continue
            } else {
                var t188 int
                var inline226 int = ref_get__Ref_3int(i__0)
                t188 = inline226
                var t189 bool
                var inline223 int = 6
                var inline224 bool = t188 == inline223
                t189 = inline224
                if t189 {
                    continue
                } else {
                    var t187 int
                    var inline221 int = ref_get__Ref_3int(i__0)
                    t187 = inline221
                    var inline218 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t187)
                    _goml_runtime_core_string_println(inline218)
                    continue
                }
            }
        } else {
            break Loop_loop180
        }
    }
    var inline239 string = "done"
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline239)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t211 string = _goml_runtime_core_int_to_string(self__69)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
