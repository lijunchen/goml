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

type TextSource struct {
    value string
}

type Ordering int32

func _goml_m_trait__impl_i_Label_i_string_i_label(self__0 string) string {
    var t799 string = "label:" + self__0
    return t799
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t802 string = self__1.value
    return t802
}

func main0() struct{} {
    var t804 TextSource = TextSource{
        value: "goml",
    }
    var t805 string
    var inline819 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t804)
    var inline820 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline819)
    t805 = inline820
    var inline816 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline816)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
