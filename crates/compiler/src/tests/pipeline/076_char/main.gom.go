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
    var c__0 rune
    var t2 string
    var d__1 rune
    var jp4 string
    var out__2 string
    c__0 = 65
    t2 = char_to_string(c__0)
    string_println(t2)
    d__1 = 98
    switch d__1 {
    case 97:
        goto b2
    case 98:
        goto b3
    default:
        goto b4
    }
    b1:
    out__2 = jp4
    string_println(out__2)
    return struct{}{}
    b2:
    jp4 = "A"
    goto b1
    b3:
    jp4 = "B"
    goto b1
    b4:
    jp4 = "?"
    goto b1
}

func main() {
    main0()
}
