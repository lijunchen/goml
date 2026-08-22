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

type Ordering int32

func main0() struct{} {
    var t805 string
    var inline833 int32 = -1
    switch inline833 {
    case -1:
        t805 = "minus one"
    case 0:
        t805 = "zero"
    case 1:
        t805 = "one"
    default:
        t805 = "other"
    }
    var inline830 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline830)
    var t806 string
    var inline828 int32 = 0
    switch inline828 {
    case -1:
        t806 = "minus one"
    case 0:
        t806 = "zero"
    case 1:
        t806 = "one"
    default:
        t806 = "other"
    }
    var inline825 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline825)
    var t807 string
    var inline823 int32 = 1
    switch inline823 {
    case -1:
        t807 = "minus one"
    case 0:
        t807 = "zero"
    case 1:
        t807 = "one"
    default:
        t807 = "other"
    }
    var inline820 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline820)
    var t808 string
    var inline818 int32 = 42
    switch inline818 {
    case -1:
        t808 = "minus one"
    case 0:
        t808 = "zero"
    case 1:
        t808 = "one"
    default:
        t808 = "other"
    }
    var inline815 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline815)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
