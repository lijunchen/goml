package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int32 = 1
    var t3 int32 = x__0 + 1
    x__0 = t3
    var t4 string = int32_to_string(x__0)
    string_println(t4)
    return struct{}{}
}

func main() {
    main0()
}
