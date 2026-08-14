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
    var t411 string
    var inline446 string = char_to_string(c__0)
    t411 = inline446
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t411)
    _goml_runtime_core_string_println(inline443)
    var d__1 rune = 98
    var jp413 string
    switch d__1 {
    case 97:
        jp413 = "A"
    case 98:
        jp413 = "B"
    default:
        jp413 = "?"
    }
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp413)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func char_to_string(value__29 rune) string {
    var t426 uint32 = uint32(rune(value__29))
    var t427 bool
    var inline454 bool = t426 <= 1114111
    if inline454 {
        var inline455 bool = t426 >= 55296
        var inline457 bool
        if inline455 {
            var inline459 bool = t426 <= 57343
            inline457 = inline459
        } else {
            inline457 = false
        }
        var inline458 bool = !inline457
        t427 = inline458
    } else {
        t427 = false
    }
    if t427 {
        var t428 string = _goml_runtime_core_char_to_string(value__29)
        return t428
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
