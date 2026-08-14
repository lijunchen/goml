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
    var t190 string
    var inline225 string = char_to_string(c__0)
    t190 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline222)
    var d__1 rune = 98
    var jp192 string
    switch d__1 {
    case 97:
        jp192 = "A"
    case 98:
        jp192 = "B"
    default:
        jp192 = "?"
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp192)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t205 uint32 = uint32(rune(value__29))
    var t206 bool
    var inline233 bool = t205 <= 1114111
    if inline233 {
        var inline234 bool = t205 >= 55296
        var inline236 bool
        if inline234 {
            var inline238 bool = t205 <= 57343
            inline236 = inline238
        } else {
            inline236 = false
        }
        var inline237 bool = !inline236
        t206 = inline237
    } else {
        t206 = false
    }
    if t206 {
        var t207 string = _goml_runtime_core_char_to_string(value__29)
        return t207
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
