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
    var inline266 int = 0
    var inline267 *ref_int_x = ref__Ref_3int(inline266)
    sum__0 = inline267
    var i__1 *ref_int_x
    var inline263 int = 0
    var inline264 *ref_int_x = ref__Ref_3int(inline263)
    i__1 = inline264
    Loop_loop194:
    for {
        var t195 int
        var inline243 int = ref_get__Ref_3int(i__1)
        t195 = inline243
        var t196 bool = t195 < 20
        if t196 {
            var t197 int
            var inline241 int = ref_get__Ref_3int(i__1)
            t197 = inline241
            var t198 int = t197 + 1
            ref_set__Ref_3int(i__1, t198)
            var t203 int
            var inline237 int = ref_get__Ref_3int(i__1)
            t203 = inline237
            var t204 bool = t203 > 5
            if t204 {
                break Loop_loop194
            } else {
                var t200 int
                var inline235 int = ref_get__Ref_3int(sum__0)
                t200 = inline235
                var t201 int
                var inline233 int = ref_get__Ref_3int(i__1)
                t201 = inline233
                var t202 int = t200 + t201
                ref_set__Ref_3int(sum__0, t202)
                continue
            }
        } else {
            break Loop_loop194
        }
    }
    var inline259 string = "sum: "
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline259)
    _goml_runtime_core_string_print(inline260)
    var t192 int
    var inline257 int = ref_get__Ref_3int(sum__0)
    t192 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t192)
    _goml_runtime_core_string_println(inline254)
    var inline250 string = "i at break: "
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline250)
    _goml_runtime_core_string_print(inline251)
    var t193 int
    var inline248 int = ref_get__Ref_3int(i__1)
    t193 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t223 string = _goml_runtime_core_int_to_string(self__67)
    return t223
}

func main() {
    main0()
}
