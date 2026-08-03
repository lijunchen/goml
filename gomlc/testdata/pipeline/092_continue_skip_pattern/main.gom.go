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
    var inline207 int = 0
    var inline208 *ref_int_x = ref__Ref_3int(inline207)
    i__0 = inline208
    Loop_loop144:
    for {
        var t145 int
        var inline201 int = ref_get__Ref_3int(i__0)
        t145 = inline201
        var t146 bool = t145 < 8
        if t146 {
            var t147 int
            var inline199 int = ref_get__Ref_3int(i__0)
            t147 = inline199
            var t148 int = t147 + 1
            ref_set__Ref_3int(i__0, t148)
            var t154 int
            var inline195 int = ref_get__Ref_3int(i__0)
            t154 = inline195
            var t155 bool
            var inline192 int = 3
            var inline193 bool = t154 == inline192
            t155 = inline193
            if t155 {
                continue
            } else {
                var t152 int
                var inline190 int = ref_get__Ref_3int(i__0)
                t152 = inline190
                var t153 bool
                var inline187 int = 6
                var inline188 bool = t152 == inline187
                t153 = inline188
                if t153 {
                    continue
                } else {
                    var t151 int
                    var inline185 int = ref_get__Ref_3int(i__0)
                    t151 = inline185
                    var inline182 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t151)
                    _goml_runtime_core_string_println(inline182)
                    continue
                }
            }
        } else {
            break Loop_loop144
        }
    }
    var inline203 string = "done"
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline203)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t175 string = _goml_runtime_core_int_to_string(self__69)
    return t175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
