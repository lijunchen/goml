package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var c__0 rune = 65
    var t3 string = char_to_string(c__0)
    println__T_string(t3)
    var d__1 rune = 98
    var jp5 string
    switch d__1 {
    case 97:
        jp5 = "A"
    case 98:
        jp5 = "B"
    default:
        jp5 = "?"
    }
    var out__2 string = jp5
    println__T_string(out__2)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
