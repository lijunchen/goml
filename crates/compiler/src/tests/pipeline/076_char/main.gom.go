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
    var t61 string = _goml_m_inherent_i_char_i_char_i_to__string(c__0)
    println__T_string(t61)
    var d__1 rune = 98
    var jp63 string
    switch d__1 {
    case 97:
        jp63 = "A"
    case 98:
        jp63 = "B"
    default:
        jp63 = "?"
    }
    var out__2 string = jp63
    println__T_string(out__2)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv68 string
    var t69 string = _goml_runtime_core_char_to_string(self__3)
    retv68 = t69
    return retv68
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv71 string
    retv71 = self__34
    return retv71
}

func main() {
    main0()
}
