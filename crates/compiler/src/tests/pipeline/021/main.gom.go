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

func match_int(n__0 int32) int32 {
    var retv8 int32
    var jp10 int32
    switch n__0 {
    case 0:
        jp10 = 10
    case 1:
        jp10 = 20
    default:
        jp10 = 30
    }
    retv8 = jp10
    return retv8
}

func wildcard_first(n__1 int32) int32 {
    var retv12 int32
    retv12 = 40
    return retv12
}

func wildcard_middle(n__2 int32) int32 {
    var retv14 int32
    var jp16 int32
    switch n__2 {
    case 2:
        jp16 = 90
    case 3:
        jp16 = 100
    default:
        jp16 = 100
    }
    retv14 = jp16
    return retv14
}

func repeated(n__3 int32) int32 {
    var retv18 int32
    var jp20 int32
    switch n__3 {
    case 1:
        jp20 = 60
    default:
        jp20 = 80
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var t22 int32 = match_int(0)
    var t23 string = int32_to_string(t22)
    string_println(t23)
    var t24 int32 = match_int(5)
    var t25 string = int32_to_string(t24)
    string_println(t25)
    var t26 int32 = wildcard_first(0)
    var t27 string = int32_to_string(t26)
    string_println(t27)
    var t28 int32 = wildcard_first(2)
    var t29 string = int32_to_string(t28)
    string_println(t29)
    var t30 int32 = wildcard_middle(2)
    var t31 string = int32_to_string(t30)
    string_println(t31)
    var t32 int32 = wildcard_middle(3)
    var t33 string = int32_to_string(t32)
    string_println(t33)
    var t34 int32 = repeated(1)
    var t35 string = int32_to_string(t34)
    string_println(t35)
    var t36 int32 = repeated(3)
    var t37 string = int32_to_string(t36)
    string_println(t37)
    return struct{}{}
}

func main() {
    main0()
}
