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
    Loop_loop179:
    for {
        var t180 int
        var inline228 int = ref_get__Ref_3int(i__0)
        t180 = inline228
        var t181 bool = t180 < 10
        if t181 {
            var t182 int
            var inline226 int = ref_get__Ref_3int(i__0)
            t182 = inline226
            var t183 int = t182 + 1
            ref_set__Ref_3int(i__0, t183)
            var t186 int
            var inline222 int = ref_get__Ref_3int(i__0)
            t186 = inline222
            var t187 bool
            var inline219 int = 5
            var inline220 bool = t186 == inline219
            t187 = inline220
            if t187 {
                continue
            } else {
                var t185 int
                var inline217 int = ref_get__Ref_3int(i__0)
                t185 = inline217
                var inline214 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t185)
                _goml_runtime_core_string_println(inline214)
                continue
            }
        } else {
            break Loop_loop179
        }
    }
    var inline230 string = "done"
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline230)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t207 string = _goml_runtime_core_int_to_string(self__67)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
