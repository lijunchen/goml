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

type Ordering int32

func main0() struct{} {
    var a__0 *ref_int_x
    var inline438 int = 1
    var inline439 *ref_int_x = ref__Ref_3int(inline438)
    a__0 = inline439
    var c__2 *ref_int_x
    var inline435 int = 1
    var inline436 *ref_int_x = ref__Ref_3int(inline435)
    c__2 = inline436
    var t411 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline432 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t411)
    _goml_runtime_core_string_println(inline432)
    var t412 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline429 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t412)
    _goml_runtime_core_string_println(inline429)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t421 string = _goml_runtime_core_bool_to_string(self__148)
    return t421
}

func main() {
    main0()
}
