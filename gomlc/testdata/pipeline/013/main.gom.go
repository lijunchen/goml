package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
    return struct{}{}
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
    var s__0 string = "abcde"
    var inline811 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_println(inline811)
    var inline808 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_print(inline808)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
