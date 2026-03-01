package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var retv4 [3]int32
    var t5 [3]int32 = [3]int32{1, 2, 3}
    retv4 = t5
    return retv4
}

func main0() struct{} {
    make_array()
    print__T_string("array literal")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func main() {
    main0()
}
