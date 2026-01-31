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
    var ret3 struct{}
    var c__0 rune = 65
    var t2 string = char_to_string(c__0)
    string_println(t2)
    var d__1 rune = 98
    var out__2 string
    switch d__1 {
    case 97:
        out__2 = "A"
    case 98:
        out__2 = "B"
    default:
        out__2 = "?"
    }
    string_println(out__2)
    ret3 = struct{}{}
    return ret3
}

func main() {
    main0()
}
