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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() bool {
    var x796 Color = Blue
    var x797 Color = Blue
    switch x797 {
    case Red:
        switch x796 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x796 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x796 {
        case Blue:
            var inline816 bool = true
            var inline817 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline816)
            _goml_runtime_core_string_print(inline817)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t814 string = _goml_runtime_core_bool_to_string(self__401)
    return t814
}

func main() {
    main0()
}
