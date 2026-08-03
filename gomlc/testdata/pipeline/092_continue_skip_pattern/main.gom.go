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
    var inline248 int = 0
    var inline249 *ref_int_x = ref__Ref_3int(inline248)
    i__0 = inline249
    Loop_loop185:
    for {
        var t186 int
        var inline242 int = ref_get__Ref_3int(i__0)
        t186 = inline242
        var t187 bool = t186 < 8
        if t187 {
            var t188 int
            var inline240 int = ref_get__Ref_3int(i__0)
            t188 = inline240
            var t189 int = t188 + 1
            ref_set__Ref_3int(i__0, t189)
            var t195 int
            var inline236 int = ref_get__Ref_3int(i__0)
            t195 = inline236
            var t196 bool
            var inline233 int = 3
            var inline234 bool = t195 == inline233
            t196 = inline234
            if t196 {
                continue
            } else {
                var t193 int
                var inline231 int = ref_get__Ref_3int(i__0)
                t193 = inline231
                var t194 bool
                var inline228 int = 6
                var inline229 bool = t193 == inline228
                t194 = inline229
                if t194 {
                    continue
                } else {
                    var t192 int
                    var inline226 int = ref_get__Ref_3int(i__0)
                    t192 = inline226
                    var inline223 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t192)
                    _goml_runtime_core_string_println(inline223)
                    continue
                }
            }
        } else {
            break Loop_loop185
        }
    }
    var inline244 string = "done"
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline244)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t216 string = _goml_runtime_core_int_to_string(self__69)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
