package main

import (
    "fmt"
    "unicode/utf8"
)

func char_to_string(x rune) string {
    if !utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var c__0 rune = 65
    var t3 string = char_to_string(c__0)
    string_println(t3)
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
    string_println(out__2)
    return struct{}{}
}

func main() {
    main0()
}
