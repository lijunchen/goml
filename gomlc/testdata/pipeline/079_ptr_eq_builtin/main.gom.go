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
    var inline212 int = 1
    var inline213 *ref_int_x = ref__Ref_3int(inline212)
    a__0 = inline213
    var c__2 *ref_int_x
    var inline209 int = 1
    var inline210 *ref_int_x = ref__Ref_3int(inline209)
    c__2 = inline210
    var t185 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline206 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline206)
    var t186 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t186)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t195 string = _goml_runtime_core_bool_to_string(self__64)
    return t195
}

func main() {
    main0()
}
