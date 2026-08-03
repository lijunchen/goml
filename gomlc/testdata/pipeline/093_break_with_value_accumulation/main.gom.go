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
    var inline220 int = 0
    var inline221 *ref_int_x = ref__Ref_3int(inline220)
    sum__0 = inline221
    var i__1 *ref_int_x
    var inline217 int = 0
    var inline218 *ref_int_x = ref__Ref_3int(inline217)
    i__1 = inline218
    Loop_loop148:
    for {
        var t149 int
        var inline197 int = ref_get__Ref_3int(i__1)
        t149 = inline197
        var t150 bool = t149 < 20
        if t150 {
            var t151 int
            var inline195 int = ref_get__Ref_3int(i__1)
            t151 = inline195
            var t152 int = t151 + 1
            ref_set__Ref_3int(i__1, t152)
            var t157 int
            var inline191 int = ref_get__Ref_3int(i__1)
            t157 = inline191
            var t158 bool = t157 > 5
            if t158 {
                break Loop_loop148
            } else {
                var t154 int
                var inline189 int = ref_get__Ref_3int(sum__0)
                t154 = inline189
                var t155 int
                var inline187 int = ref_get__Ref_3int(i__1)
                t155 = inline187
                var t156 int = t154 + t155
                ref_set__Ref_3int(sum__0, t156)
                continue
            }
        } else {
            break Loop_loop148
        }
    }
    var inline213 string = "sum: "
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline213)
    _goml_runtime_core_string_print(inline214)
    var t146 int
    var inline211 int = ref_get__Ref_3int(sum__0)
    t146 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t146)
    _goml_runtime_core_string_println(inline208)
    var inline204 string = "i at break: "
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline204)
    _goml_runtime_core_string_print(inline205)
    var t147 int
    var inline202 int = ref_get__Ref_3int(i__1)
    t147 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t147)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t177 string = _goml_runtime_core_int_to_string(self__69)
    return t177
}

func main() {
    main0()
}
