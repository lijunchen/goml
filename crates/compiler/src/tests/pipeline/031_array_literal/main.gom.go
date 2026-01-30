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
    print__T_string("array literal")
    ret4 = struct{}{}
    return ret4
}

func print__T_string(value__0 string) struct{} {
    var ret5 struct{}
    ret5 = string_print(value__0)
    return ret5
}

func main() {
    main0()
}
