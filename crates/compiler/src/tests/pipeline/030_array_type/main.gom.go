package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

func main0() struct{} {
    var ret1 struct{}
    ret1 = string_print("array")
    return ret1
}

func main() {
    main0()
}
