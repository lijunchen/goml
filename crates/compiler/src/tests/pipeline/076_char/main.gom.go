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
    var _wild0 struct{}
    var d__1 rune
    var jp4 string
    var out__2 string
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            c__0 = 65
            t2 = char_to_string(c__0)
            string_println(t2)
            d__1 = 98
            switch d__1 {
            case 97:
                pc = 2
            case 98:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            out__2 = jp4
            string_println(out__2)
            return struct{}{}
        case 2:
            jp4 = "A"
            pc = 1
        case 3:
            jp4 = "B"
            pc = 1
        case 4:
            jp4 = "?"
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
