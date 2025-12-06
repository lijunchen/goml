package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var ret3 [3]int32
    ret3 = [3]int32{1, 2, 3}
    return ret3
}

func main0() struct{} {
    var ret4 struct{}
    make_array()
    string_print("array literal")
    ret4 = struct{}{}
    return ret4
}

func main() {
    main0()
}
