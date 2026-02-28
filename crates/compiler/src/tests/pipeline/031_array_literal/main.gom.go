package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var t3 [3]int32 = [3]int32{1, 2, 3}
    return t3
}

func main0() struct{} {
    make_array()
    print__T_string("array literal")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t4 struct{} = string_print(value__0)
    return t4
}

func main() {
    main0()
}
