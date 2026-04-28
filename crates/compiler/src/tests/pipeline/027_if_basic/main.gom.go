package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func choose(flag__0 bool, x__1 int32, y__2 int32) int32 {
    var retv3 int32
    var jp5 int32
    if flag__0 {
        jp5 = x__1
    } else {
        jp5 = y__2
    }
    retv3 = jp5
    return retv3
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t7 string = int32_to_string(yes__3)
    var t8 string = "yes=" + t7
    string_println(t8)
    var t9 string = int32_to_string(no__4)
    var t10 string = "no=" + t9
    string_println(t10)
    return struct{}{}
}

func main() {
    main0()
}
