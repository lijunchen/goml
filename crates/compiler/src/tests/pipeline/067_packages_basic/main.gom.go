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

type _goml_Lib_x3a__x3a_Color interface {
    is_goml_Lib_x3a__x3a_Color()
}

type Red struct {}

func (_ Red) is_goml_Lib_x3a__x3a_Color() {}

type Green struct {}

func (_ Green) is_goml_Lib_x3a__x3a_Color() {}

func main0() struct{} {
    var t1 int32 = _goml_Lib_x3a__x3a_color_to_int(Red{})
    var t2 string = int32_to_string(t1)
    string_println(t2)
    return struct{}{}
}

func _goml_Lib_x3a__x3a_color_to_int(c__0 _goml_Lib_x3a__x3a_Color) int32 {
    var retv5 int32
    var jp7 int32
    switch c__0.(type) {
    case Red:
        jp7 = 1
    case Green:
        jp7 = 2
    default:
        panic("non-exhaustive match")
    }
    retv5 = jp7
    return retv5
}

func main() {
    main0()
}
