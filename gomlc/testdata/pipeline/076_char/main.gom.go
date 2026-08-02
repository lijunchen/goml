package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var c__0 rune = 65
    var t158 string
    var inline176 string = _goml_runtime_core_char_to_string(c__0)
    t158 = inline176
    var inline173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline173)
    var d__1 rune = 98
    var jp160 string
    switch d__1 {
    case 97:
        jp160 = "A"
    case 98:
        jp160 = "B"
    default:
        jp160 = "?"
    }
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp160)
    _goml_runtime_core_string_println(inline170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
