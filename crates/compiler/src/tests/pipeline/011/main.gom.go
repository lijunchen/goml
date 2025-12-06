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
    var ret1 struct{}
    var a__0 int32 = 1
    var a__1 int32 = a__0 + 2
    var a__2 int32 = a__1 + 3
    var a__3 int32 = a__2 + 4
    var t0 string = int32_to_string(a__3)
    ret1 = string_print(t0)
    return ret1
}

func main() {
    main0()
}
