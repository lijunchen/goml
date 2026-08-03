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
    var inline166 int = 1
    var inline167 *ref_int_x = ref__Ref_3int(inline166)
    a__0 = inline167
    var c__2 *ref_int_x
    var inline163 int = 1
    var inline164 *ref_int_x = ref__Ref_3int(inline163)
    c__2 = inline164
    var t139 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline160 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t139)
    _goml_runtime_core_string_println(inline160)
    var t140 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline157 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t140)
    _goml_runtime_core_string_println(inline157)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t149 string = _goml_runtime_core_bool_to_string(self__66)
    return t149
}

func main() {
    main0()
}
