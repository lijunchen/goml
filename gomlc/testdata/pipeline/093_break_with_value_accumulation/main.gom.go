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
    var inline261 int = 0
    var inline262 *ref_int_x = ref__Ref_3int(inline261)
    sum__0 = inline262
    var i__1 *ref_int_x
    var inline258 int = 0
    var inline259 *ref_int_x = ref__Ref_3int(inline258)
    i__1 = inline259
    Loop_loop189:
    for {
        var t190 int
        var inline238 int = ref_get__Ref_3int(i__1)
        t190 = inline238
        var t191 bool = t190 < 20
        if t191 {
            var t192 int
            var inline236 int = ref_get__Ref_3int(i__1)
            t192 = inline236
            var t193 int = t192 + 1
            ref_set__Ref_3int(i__1, t193)
            var t198 int
            var inline232 int = ref_get__Ref_3int(i__1)
            t198 = inline232
            var t199 bool = t198 > 5
            if t199 {
                break Loop_loop189
            } else {
                var t195 int
                var inline230 int = ref_get__Ref_3int(sum__0)
                t195 = inline230
                var t196 int
                var inline228 int = ref_get__Ref_3int(i__1)
                t196 = inline228
                var t197 int = t195 + t196
                ref_set__Ref_3int(sum__0, t197)
                continue
            }
        } else {
            break Loop_loop189
        }
    }
    var inline254 string = "sum: "
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
    _goml_runtime_core_string_print(inline255)
    var t187 int
    var inline252 int = ref_get__Ref_3int(sum__0)
    t187 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t187)
    _goml_runtime_core_string_println(inline249)
    var inline245 string = "i at break: "
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
    _goml_runtime_core_string_print(inline246)
    var t188 int
    var inline243 int = ref_get__Ref_3int(i__1)
    t188 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t218 string = _goml_runtime_core_int_to_string(self__69)
    return t218
}

func main() {
    main0()
}
