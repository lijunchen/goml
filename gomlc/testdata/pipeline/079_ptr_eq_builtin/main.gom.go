package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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

func ptr_eq__Ref_3int(a *ref_int_x, b *ref_int_x) bool {
    return a == b
}

func main0() struct{} {
    var a__0 *ref_int_x
    var inline202 int = 1
    var inline203 *ref_int_x = ref__Ref_3int(inline202)
    a__0 = inline203
    var c__2 *ref_int_x
    var inline199 int = 1
    var inline200 *ref_int_x = ref__Ref_3int(inline199)
    c__2 = inline200
    var t175 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline196 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t175)
    _goml_runtime_core_string_println(inline196)
    var t176 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t176)
    _goml_runtime_core_string_println(inline193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t185 string = _goml_runtime_core_bool_to_string(self__64)
    return t185
}

func main() {
    main0()
}
