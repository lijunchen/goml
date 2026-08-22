package main

import (
    _goml_os "os"
)

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

type Value struct {}

type Ordering int32

func _goml_m_trait__impl_i_Base_i_Value_i_base(self__0 Value) string {
    return "B"
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    return "L"
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    return "R"
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    return "D"
}

func main0() struct{} {
    var t806 Value = Value{}
    var t807 string
    var inline826 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t806)
    var inline827 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t806)
    var inline828 string = inline826 + inline827
    var inline829 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t806)
    var inline830 string = inline828 + inline829
    var inline831 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t806)
    var inline832 string = inline830 + inline831
    t807 = inline832
    var inline823 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline823)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
