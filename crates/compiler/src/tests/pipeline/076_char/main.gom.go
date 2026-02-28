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
    var t2 string = char_to_string(c__0)
    string_println(t2)
    var d__1 rune = 98
    var jp4 string
    switch d__1 {
    case 97:
        jp4 = "A"
    case 98:
        jp4 = "B"
    default:
        jp4 = "?"
    }
    var out__2 string = jp4
    string_println(out__2)
    return struct{}{}
}

func main() {
    main0()
}
