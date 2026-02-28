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
    var t0 int32
    var t1 string
    var t2 struct{}
    t0 = _goml_Lib_x3a__x3a_color_to_int(Red{})
    t1 = int32_to_string(t0)
    t2 = string_println(t1)
    return t2
}

func _goml_Lib_x3a__x3a_color_to_int(c__0 _goml_Lib_x3a__x3a_Color) int32 {
    var jp4 int32
    switch c__0.(type) {
    case Red:
        goto b2
    case Green:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp4
    b2:
    jp4 = 1
    goto b1
    b3:
    jp4 = 2
    goto b1
}

func main() {
    main0()
}
