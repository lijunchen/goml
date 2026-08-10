package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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
    var sum__0 *ref_int_x
    var inline256 int = 0
    var inline257 *ref_int_x = ref__Ref_3int(inline256)
    sum__0 = inline257
    var i__1 *ref_int_x
    var inline253 int = 0
    var inline254 *ref_int_x = ref__Ref_3int(inline253)
    i__1 = inline254
    Loop_loop184:
    for {
        var t185 int
        var inline233 int = ref_get__Ref_3int(i__1)
        t185 = inline233
        var t186 bool = t185 < 20
        if t186 {
            var t187 int
            var inline231 int = ref_get__Ref_3int(i__1)
            t187 = inline231
            var t188 int = t187 + 1
            ref_set__Ref_3int(i__1, t188)
            var t193 int
            var inline227 int = ref_get__Ref_3int(i__1)
            t193 = inline227
            var t194 bool = t193 > 5
            if t194 {
                break Loop_loop184
            } else {
                var t190 int
                var inline225 int = ref_get__Ref_3int(sum__0)
                t190 = inline225
                var t191 int
                var inline223 int = ref_get__Ref_3int(i__1)
                t191 = inline223
                var t192 int = t190 + t191
                ref_set__Ref_3int(sum__0, t192)
                continue
            }
        } else {
            break Loop_loop184
        }
    }
    var inline249 string = "sum: "
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
    _goml_runtime_core_string_print(inline250)
    var t182 int
    var inline247 int = ref_get__Ref_3int(sum__0)
    t182 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t182)
    _goml_runtime_core_string_println(inline244)
    var inline240 string = "i at break: "
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
    _goml_runtime_core_string_print(inline241)
    var t183 int
    var inline238 int = ref_get__Ref_3int(i__1)
    t183 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t183)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t213 string = _goml_runtime_core_int_to_string(self__67)
    return t213
}

func main() {
    main0()
}
