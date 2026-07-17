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
    var t64 string = _goml_m_inherent_i_char_i_char_i_to__string(c__0)
    println__T_string(t64)
    var d__1 rune = 98
    var jp66 string
    switch d__1 {
    case 97:
        jp66 = "A"
    case 98:
        jp66 = "B"
    default:
        jp66 = "?"
    }
    var out__2 string = jp66
    println__T_string(out__2)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv71 string
    var t72 string = _goml_runtime_core_char_to_string(self__6)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv74 string
    retv74 = self__37
    return retv74
}

func main() {
    main0()
}
