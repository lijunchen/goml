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
    Loop_loop184:
    for {
        var t185 int
        var inline233 int = ref_get__Ref_3int(i__0)
        t185 = inline233
        var t186 bool = t185 < 10
        if t186 {
            var t191 int
            var inline231 int = ref_get__Ref_3int(i__0)
            t191 = inline231
            var t192 bool
            var inline228 int = 5
            var inline229 bool = t191 == inline228
            t192 = inline229
            if t192 {
                break Loop_loop184
            } else {
                var t188 int
                var inline226 int = ref_get__Ref_3int(i__0)
                t188 = inline226
                var inline223 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
                _goml_runtime_core_string_println(inline223)
                var t189 int
                var inline221 int = ref_get__Ref_3int(i__0)
                t189 = inline221
                var t190 int = t189 + 1
                ref_set__Ref_3int(i__0, t190)
                continue
            }
        } else {
            break Loop_loop184
        }
    }
    var inline235 string = "done"
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t212 string = _goml_runtime_core_int_to_string(self__69)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
