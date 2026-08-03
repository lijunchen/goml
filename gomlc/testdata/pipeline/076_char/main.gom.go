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
    var t139 string
    var inline174 string = char_to_string(c__0)
    t139 = inline174
    var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t139)
    _goml_runtime_core_string_println(inline171)
    var d__1 rune = 98
    var jp141 string
    switch d__1 {
    case 97:
        jp141 = "A"
    case 98:
        jp141 = "B"
    default:
        jp141 = "?"
    }
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp141)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t154 uint32 = uint32(rune(value__29))
    var t155 bool
    var inline182 bool = t154 <= 1114111
    if inline182 {
        var inline183 bool = t154 >= 55296
        var inline185 bool
        if inline183 {
            var inline187 bool = t154 <= 57343
            inline185 = inline187
        } else {
            inline185 = false
        }
        var inline186 bool = !inline185
        t155 = inline186
    } else {
        t155 = false
    }
    if t155 {
        var t156 string = _goml_runtime_core_char_to_string(value__29)
        return t156
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
