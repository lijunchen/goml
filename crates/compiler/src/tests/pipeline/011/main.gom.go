package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    var a__0 int32
    var a__1 int32
    var a__2 int32
    var a__3 int32
    var t0 string
    var t1 struct{}
    a__0 = 1
    a__1 = a__0 + 2
    a__2 = a__1 + 3
    a__3 = a__2 + 4
    t0 = int32_to_string(a__3)
    t1 = string_print(t0)
    return t1
}

func main() {
    main0()
}
