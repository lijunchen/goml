package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var ret7 [3]int32
    ret7 = [3]int32{1, 2, 3}
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    make_array()
    print__T_string("array literal")
    ret8 = struct{}{}
    return ret8
}

func print__T_string(value__0 string) struct{} {
    var ret9 struct{}
    ret9 = string_print(value__0)
    return ret9
}

func main() {
    main0()
}
