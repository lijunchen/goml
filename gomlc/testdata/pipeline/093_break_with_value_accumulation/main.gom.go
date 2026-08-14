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
    var inline271 int = 0
    var inline272 *ref_int_x = ref__Ref_3int(inline271)
    sum__0 = inline272
    var i__1 *ref_int_x
    var inline268 int = 0
    var inline269 *ref_int_x = ref__Ref_3int(inline268)
    i__1 = inline269
    Loop_loop199:
    for {
        var t200 int
        var inline248 int = ref_get__Ref_3int(i__1)
        t200 = inline248
        var t201 bool = t200 < 20
        if t201 {
            var t202 int
            var inline246 int = ref_get__Ref_3int(i__1)
            t202 = inline246
            var t203 int = t202 + 1
            ref_set__Ref_3int(i__1, t203)
            var t208 int
            var inline242 int = ref_get__Ref_3int(i__1)
            t208 = inline242
            var t209 bool = t208 > 5
            if t209 {
                break Loop_loop199
            } else {
                var t205 int
                var inline240 int = ref_get__Ref_3int(sum__0)
                t205 = inline240
                var t206 int
                var inline238 int = ref_get__Ref_3int(i__1)
                t206 = inline238
                var t207 int = t205 + t206
                ref_set__Ref_3int(sum__0, t207)
                continue
            }
        } else {
            break Loop_loop199
        }
    }
    var inline264 string = "sum: "
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline264)
    _goml_runtime_core_string_print(inline265)
    var t197 int
    var inline262 int = ref_get__Ref_3int(sum__0)
    t197 = inline262
    var inline259 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t197)
    _goml_runtime_core_string_println(inline259)
    var inline255 string = "i at break: "
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline255)
    _goml_runtime_core_string_print(inline256)
    var t198 int
    var inline253 int = ref_get__Ref_3int(i__1)
    t198 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t228 string = _goml_runtime_core_int_to_string(self__67)
    return t228
}

func main() {
    main0()
}
