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
    var inline441 int = 1
    var inline442 *ref_int_x = ref__Ref_3int(inline441)
    a__0 = inline442
    var c__2 *ref_int_x
    var inline438 int = 1
    var inline439 *ref_int_x = ref__Ref_3int(inline438)
    c__2 = inline439
    var t414 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline435 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t414)
    _goml_runtime_core_string_println(inline435)
    var t415 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline432 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t415)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t424 string = _goml_runtime_core_bool_to_string(self__148)
    return t424
}

func main() {
    main0()
}
