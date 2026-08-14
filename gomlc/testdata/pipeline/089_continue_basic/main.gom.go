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
    var inline244 int = 0
    var inline245 *ref_int_x = ref__Ref_3int(inline244)
    i__0 = inline245
    Loop_loop189:
    for {
        var t190 int
        var inline238 int = ref_get__Ref_3int(i__0)
        t190 = inline238
        var t191 bool = t190 < 10
        if t191 {
            var t192 int
            var inline236 int = ref_get__Ref_3int(i__0)
            t192 = inline236
            var t193 int = t192 + 1
            ref_set__Ref_3int(i__0, t193)
            var t196 int
            var inline232 int = ref_get__Ref_3int(i__0)
            t196 = inline232
            var t197 bool
            var inline229 int = 5
            var inline230 bool = t196 == inline229
            t197 = inline230
            if t197 {
                continue
            } else {
                var t195 int
                var inline227 int = ref_get__Ref_3int(i__0)
                t195 = inline227
                var inline224 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t195)
                _goml_runtime_core_string_println(inline224)
                continue
            }
        } else {
            break Loop_loop189
        }
    }
    var inline240 string = "done"
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t217 string = _goml_runtime_core_int_to_string(self__67)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
