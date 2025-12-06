package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret2 struct{}
    var poem__0 string = "roses are red\nviolets are blue\n\"quotes\" stay quoted\nbackslash \\\\\\\\ stays too"
    var trailing_blank__1 string = "line one\n\nline three"
    string_println(poem__0)
    string_println(trailing_blank__1)
    ret2 = struct{}{}
    return ret2
}

func main() {
    main0()
}
