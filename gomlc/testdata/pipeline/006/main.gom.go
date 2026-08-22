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

func main0() struct{} {
    var x796 bool = true
    var x797 bool = false
    switch x796 {
    case true:
        var inline813 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x797)
        _goml_runtime_core_string_print(inline813)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x799 bool = true
    var x800 bool = true
    switch x799 {
    case true:
        var inline816 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x800)
        _goml_runtime_core_string_print(inline816)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t811 string = _goml_runtime_core_bool_to_string(self__401)
    return t811
}

func main() {
    main0()
}
