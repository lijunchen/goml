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
    var inline198 int = 0
    var inline199 *ref_int_x = ref__Ref_3int(inline198)
    i__0 = inline199
    Loop_loop143:
    for {
        var t144 int
        var inline192 int = ref_get__Ref_3int(i__0)
        t144 = inline192
        var t145 bool = t144 < 10
        if t145 {
            var t150 int
            var inline190 int = ref_get__Ref_3int(i__0)
            t150 = inline190
            var t151 bool
            var inline187 int = 5
            var inline188 bool = t150 == inline187
            t151 = inline188
            if t151 {
                break Loop_loop143
            } else {
                var t147 int
                var inline185 int = ref_get__Ref_3int(i__0)
                t147 = inline185
                var inline182 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t147)
                _goml_runtime_core_string_println(inline182)
                var t148 int
                var inline180 int = ref_get__Ref_3int(i__0)
                t148 = inline180
                var t149 int = t148 + 1
                ref_set__Ref_3int(i__0, t149)
                continue
            }
        } else {
            break Loop_loop143
        }
    }
    var inline194 string = "done"
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline194)
    _goml_runtime_core_string_println(inline195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t171 string = _goml_runtime_core_int_to_string(self__69)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
