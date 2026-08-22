package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
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

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Ordering int32

func main0() struct{} {
    var a__0 *ref_int_x
    var inline826 int = 1
    var inline827 *ref_int_x = ref__Ref_3int(inline826)
    a__0 = inline827
    var c__2 *ref_int_x
    var inline823 int = 1
    var inline824 *ref_int_x = ref__Ref_3int(inline823)
    c__2 = inline824
    var t799 bool = ptr_eq__Ref_3int(a__0, a__0)
    var inline820 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t799)
    _goml_runtime_core_string_println(inline820)
    var t800 bool = ptr_eq__Ref_3int(a__0, c__2)
    var inline817 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t800)
    _goml_runtime_core_string_println(inline817)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t809 string = _goml_runtime_core_bool_to_string(self__401)
    return t809
}

func main() {
    main0()
}
