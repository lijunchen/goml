package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    string_println("ok")
    return struct{}{}
}

func main() {
    main0()
}
