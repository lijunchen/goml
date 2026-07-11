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
    var t25 string = _goml_m_inherent_i_char_i_char_i_to__string(c__0)
    println__T_string(t25)
    var d__1 rune = 98
    var jp27 string
    switch d__1 {
    case 97:
        jp27 = "A"
    case 98:
        jp27 = "B"
    default:
        jp27 = "?"
    }
    var out__2 string = jp27
    println__T_string(out__2)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t29 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t29)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv32 string
    var t33 string = _goml_runtime_core_char_to_string(self__3)
    retv32 = t33
    return retv32
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv35 string
    retv35 = self__9
    return retv35
}

func main() {
    main0()
}
