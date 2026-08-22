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

type Light__i32 int32

const (
    Light__i32_Red Light__i32 = 0
    Light__i32_Green Light__i32 = 1
)

type Light__string int32

const (
    Light__string_Red Light__string = 0
    Light__string_Green Light__string = 1
)

func main0() struct{} {
    var t807 Light__i32
    t807 = Light__i32_Green
    var t808 string
    switch t807 {
    case Light__i32_Red:
        t808 = "ri"
    case Light__i32_Green:
        t808 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline830 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline830)
    var t809 Light__string
    t809 = Light__string_Red
    var t810 string
    switch t809 {
    case Light__string_Red:
        t810 = "rs"
    case Light__string_Green:
        t810 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline825 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline825)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
