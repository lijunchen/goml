package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var poem__0 string
    var trailing_blank__1 string
    var _wild0 struct{}
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            poem__0 = "roses are red\nviolets are blue\n\"quotes\" stay quoted\nbackslash \\\\\\\\ stays too"
            trailing_blank__1 = "line one\n\nline three"
            string_println(poem__0)
            string_println(trailing_blank__1)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
