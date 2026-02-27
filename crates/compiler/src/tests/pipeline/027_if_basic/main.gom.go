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

func choose(flag__0 bool, x__1 int32, y__2 int32) int32 {
    var ret7 int32
    if flag__0 {
        ret7 = x__1
    } else {
        ret7 = y__2
    }
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t4 string = int32_to_string(yes__3)
    var t3 string = "yes=" + t4
    string_println(t3)
    var t6 string = int32_to_string(no__4)
    var t5 string = "no=" + t6
    string_println(t5)
    ret8 = struct{}{}
    return ret8
}

func main() {
    main0()
}
