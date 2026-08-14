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
    var inline217 int = 1
    var inline218 *ref_int_x = ref__Ref_3int(inline217)
    a__0 = inline218
    var c__2 *ref_int_x
    var inline214 int = 1
    var inline215 *ref_int_x = ref__Ref_3int(inline214)
    c__2 = inline215
    var t190 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline211)
    var t191 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t200 string = _goml_runtime_core_bool_to_string(self__64)
    return t200
}

func main() {
    main0()
}
