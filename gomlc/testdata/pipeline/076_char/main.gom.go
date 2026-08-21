package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var c__0 rune = 65
    var t414 string
    var inline449 string = char_to_string(c__0)
    t414 = inline449
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline446)
    var d__1 rune = 98
    var jp416 string
    switch d__1 {
    case 97:
        jp416 = "A"
    case 98:
        jp416 = "B"
    default:
        jp416 = "?"
    }
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp416)
    _goml_runtime_core_string_println(inline443)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func char_to_string(value__29 rune) string {
    var t429 uint32 = uint32(rune(value__29))
    var t430 bool
    var inline457 bool = t429 <= 1114111
    if inline457 {
        var inline458 bool = t429 >= 55296
        var inline460 bool
        if inline458 {
            var inline462 bool = t429 <= 57343
            inline460 = inline462
        } else {
            inline460 = false
        }
        var inline461 bool = !inline460
        t430 = inline461
    } else {
        t430 = false
    }
    if t430 {
        var t431 string = _goml_runtime_core_char_to_string(value__29)
        return t431
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
