package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t0 struct{}
    t0 = string_println("ok")
    return t0
}

func main() {
    main0()
}
