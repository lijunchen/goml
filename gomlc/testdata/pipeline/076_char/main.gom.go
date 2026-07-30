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
    var t111 string = _goml_m_inherent_i_char_i_char_i_to__string(c__0)
    println__T_string(t111)
    var d__1 rune = 98
    var jp113 string
    switch d__1 {
    case 97:
        jp113 = "A"
    case 98:
        jp113 = "B"
    default:
        jp113 = "?"
    }
    var out__2 string = jp113
    println__T_string(out__2)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv118 string
    var t119 string = _goml_runtime_core_char_to_string(self__7)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
