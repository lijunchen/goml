package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret0 struct{}
    ret0 = string_println("ok")
    return ret0
}

func main() {
    main0()
}
