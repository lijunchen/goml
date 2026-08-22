package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
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
    var c__0 rune = 65
    var t799 string
    var inline834 string = char_to_string(c__0)
    t799 = inline834
    var inline831 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t799)
    _goml_runtime_core_string_println(inline831)
    var d__1 rune = 98
    var jp801 string
    switch d__1 {
    case 97:
        jp801 = "A"
    case 98:
        jp801 = "B"
    default:
        jp801 = "?"
    }
    var inline828 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp801)
    _goml_runtime_core_string_println(inline828)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func char_to_string(value__282 rune) string {
    var t814 uint32 = uint32(rune(value__282))
    var t815 bool
    var inline842 bool = t814 <= 1114111
    if inline842 {
        var inline843 bool = t814 >= 55296
        var inline845 bool
        if inline843 {
            var inline847 bool = t814 <= 57343
            inline845 = inline847
        } else {
            inline845 = false
        }
        var inline846 bool = !inline845
        t815 = inline846
    } else {
        t815 = false
    }
    if t815 {
        var t816 string = _goml_runtime_core_char_to_string(value__282)
        return t816
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
