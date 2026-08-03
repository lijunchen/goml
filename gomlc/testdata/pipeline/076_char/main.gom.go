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
    var t180 string
    var inline215 string = char_to_string(c__0)
    t180 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline212)
    var d__1 rune = 98
    var jp182 string
    switch d__1 {
    case 97:
        jp182 = "A"
    case 98:
        jp182 = "B"
    default:
        jp182 = "?"
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp182)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t195 uint32 = uint32(rune(value__29))
    var t196 bool
    var inline223 bool = t195 <= 1114111
    if inline223 {
        var inline224 bool = t195 >= 55296
        var inline226 bool
        if inline224 {
            var inline228 bool = t195 <= 57343
            inline226 = inline228
        } else {
            inline226 = false
        }
        var inline227 bool = !inline226
        t196 = inline227
    } else {
        t196 = false
    }
    if t196 {
        var t197 string = _goml_runtime_core_char_to_string(value__29)
        return t197
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
