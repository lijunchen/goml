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
    var t806 string
    var inline840 float64 = 0
    switch inline840 {
    case 0:
        t806 = "zero"
    case 1:
        t806 = "one"
    case -1:
        t806 = "minus one"
    case 3.14:
        t806 = "pi"
    default:
        t806 = "other"
    }
    var inline837 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline837)
    var t807 string
    var inline835 float64 = 1
    switch inline835 {
    case 0:
        t807 = "zero"
    case 1:
        t807 = "one"
    case -1:
        t807 = "minus one"
    case 3.14:
        t807 = "pi"
    default:
        t807 = "other"
    }
    var inline832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline832)
    var t808 float64 = -1
    var t809 string
    switch t808 {
    case 0:
        t809 = "zero"
    case 1:
        t809 = "one"
    case -1:
        t809 = "minus one"
    case 3.14:
        t809 = "pi"
    default:
        t809 = "other"
    }
    var inline828 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline828)
    var t810 string
    var inline826 float64 = 3.14
    switch inline826 {
    case 0:
        t810 = "zero"
    case 1:
        t810 = "one"
    case -1:
        t810 = "minus one"
    case 3.14:
        t810 = "pi"
    default:
        t810 = "other"
    }
    var inline823 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline823)
    var t811 string
    var inline821 float64 = 42
    switch inline821 {
    case 0:
        t811 = "zero"
    case 1:
        t811 = "one"
    case -1:
        t811 = "minus one"
    case 3.14:
        t811 = "pi"
    default:
        t811 = "other"
    }
    var inline818 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline818)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
