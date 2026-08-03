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
            var t187 int
            var inline231 int = ref_get__Ref_3int(i__0)
            t187 = inline231
            var t188 int = t187 + 1
            ref_set__Ref_3int(i__0, t188)
            var t191 int
            var inline227 int = ref_get__Ref_3int(i__0)
            t191 = inline227
            var t192 bool
            var inline224 int = 5
            var inline225 bool = t191 == inline224
            t192 = inline225
            if t192 {
                continue
            } else {
                var t190 int
                var inline222 int = ref_get__Ref_3int(i__0)
                t190 = inline222
                var inline219 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t190)
                _goml_runtime_core_string_println(inline219)
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
