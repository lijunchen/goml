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

func classify(x__0 int32) string {
    var t806 bool = x__0 < 0
    if t806 {
        return "negative"
    } else {
        var t809 bool = 0 < x__0
        if t809 {
            return "positive"
        } else {
            return "zero"
        }
    }
}

func main0() struct{} {
    var first__4 string = classify(-42)
    var second__5 string = classify(0)
    var third__6 string = classify(17)
    var shape1__7 string
    var inline860 int32 = 1
    var inline861 int32 = 2
    var inline862 int32 = 3
    var inline863 bool = inline860 < inline861
    if inline863 {
        var inline864 bool = inline861 < inline862
        if inline864 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline865 bool = inline860 < inline862
        if inline865 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline853 int32 = 3
    var inline854 int32 = 2
    var inline855 int32 = 1
    var inline856 bool = inline853 < inline854
    if inline856 {
        var inline857 bool = inline854 < inline855
        if inline857 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline858 bool = inline853 < inline855
        if inline858 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline846 int32 = 2
    var inline847 int32 = 3
    var inline848 int32 = 2
    var inline849 bool = inline846 < inline847
    if inline849 {
        var inline850 bool = inline847 < inline848
        if inline850 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline851 bool = inline846 < inline848
        if inline851 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline843 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline843)
    var inline840 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline840)
    var inline837 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline837)
    var inline834 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline834)
    var inline831 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline831)
    var inline828 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline828)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
