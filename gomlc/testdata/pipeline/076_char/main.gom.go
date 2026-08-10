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
    var t175 string
    var inline210 string = char_to_string(c__0)
    t175 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline207)
    var d__1 rune = 98
    var jp177 string
    switch d__1 {
    case 97:
        jp177 = "A"
    case 98:
        jp177 = "B"
    default:
        jp177 = "?"
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp177)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t190 uint32 = uint32(rune(value__29))
    var t191 bool
    var inline218 bool = t190 <= 1114111
    if inline218 {
        var inline219 bool = t190 >= 55296
        var inline221 bool
        if inline219 {
            var inline223 bool = t190 <= 57343
            inline221 = inline223
        } else {
            inline221 = false
        }
        var inline222 bool = !inline221
        t191 = inline222
    } else {
        t191 = false
    }
    if t191 {
        var t192 string = _goml_runtime_core_char_to_string(value__29)
        return t192
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
