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

func main0() struct{} {
    var c__0 rune = 65
    var t185 string
    var inline220 string = char_to_string(c__0)
    t185 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline217)
    var d__1 rune = 98
    var jp187 string
    switch d__1 {
    case 97:
        jp187 = "A"
    case 98:
        jp187 = "B"
    default:
        jp187 = "?"
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp187)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t200 uint32 = uint32(rune(value__29))
    var t201 bool
    var inline228 bool = t200 <= 1114111
    if inline228 {
        var inline229 bool = t200 >= 55296
        var inline231 bool
        if inline229 {
            var inline233 bool = t200 <= 57343
            inline231 = inline233
        } else {
            inline231 = false
        }
        var inline232 bool = !inline231
        t201 = inline232
    } else {
        t201 = false
    }
    if t201 {
        var t202 string = _goml_runtime_core_char_to_string(value__29)
        return t202
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
