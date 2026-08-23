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
    var t0_lhs string = ""
    var t0_rhs string = "{{"
    var t0 string = t0_lhs + t0_rhs
    var t1 bool = t0 == "{{"
    var inline6 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1)
    _goml_runtime_core_string_println(inline6)
    var t2_lhs string = ""
    var t2_rhs string = "}}"
    var t2 string = t2_lhs + t2_rhs
    var t3 bool = t2 == "}}"
    var inline4 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t3)
    _goml_runtime_core_string_println(inline4)
    var t4_lhs string = ""
    var t4_rhs string = "{{"
    var t4 string = t4_lhs + t4_rhs
    var t5 bool = t4 == "{{"
    var inline2 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t5)
    _goml_runtime_core_string_println(inline2)
    var t6_lhs string = ""
    var t6_rhs string = "}}"
    var t6 string = t6_lhs + t6_rhs
    var t7 bool = t6 == "}}"
    var inline0 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t7)
    _goml_runtime_core_string_println(inline0)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__0 bool) string {
    var t0 string = _goml_runtime_core_bool_to_string(self__0)
    return t0
}

func main() {
    main0()
}
