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
    var inline185 int = 1
    var inline186 *ref_int_x = ref__Ref_3int(inline185)
    a__0 = inline186
    var c__2 *ref_int_x
    var inline182 int = 1
    var inline183 *ref_int_x = ref__Ref_3int(inline182)
    c__2 = inline183
    var t158 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline179 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t158)
    _goml_runtime_core_string_println(inline179)
    var t159 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline176 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t159)
    _goml_runtime_core_string_println(inline176)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t168 string = _goml_runtime_core_bool_to_string(self__37)
    return t168
}

func main() {
    main0()
}
