package main

import (
    _goml_fmt "fmt"
    "path"
    "strings"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var parts__0 []string = strings.Fields("alpha beta gamma")
    var t2 int32 = int32(len(parts__0))
    var t3 []string = parts__0[0:t2]
    var t4 string = path.Join(t3...)
    string_println(t4)
    return struct{}{}
}

func main() {
    main0()
}
