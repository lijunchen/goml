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

type goml__Lib_x3a__x3a_Color interface {
    isgoml__Lib_x3a__x3a_Color()
}

type Red struct {}

func (_ Red) isgoml__Lib_x3a__x3a_Color() {}

type Green struct {}

func (_ Green) isgoml__Lib_x3a__x3a_Color() {}

func main0() struct{} {
    var ret3 struct{}
    var t2 goml__Lib_x3a__x3a_Color = Red{}
    var t1 int32 = goml__Lib_x3a__x3a_color__to__int(t2)
    var t0 string = int32_to_string(t1)
    ret3 = string_println(t0)
    return ret3
}

func goml__Lib_x3a__x3a_color__to__int(c__0 goml__Lib_x3a__x3a_Color) int32 {
    var ret4 int32
    switch c__0.(type) {
    case Red:
        ret4 = 1
    case Green:
        ret4 = 2
    }
    return ret4
}

func main() {
    main0()
}
