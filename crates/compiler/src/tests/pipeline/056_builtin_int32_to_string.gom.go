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
    var ret1 struct{}
    var value__0 int32 = 42
    var text__1 string = int32_to_string(value__0)
    string_println(text__1)
    ret1 = struct{}{}
    return ret1
}

func main() {
    main0()
}
