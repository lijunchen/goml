package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Light__int32 int32

const (
    Light__int32_Red Light__int32 = 0
    Light__int32_Green Light__int32 = 1
)

type Light__string int32

const (
    Light__string_Red Light__string = 0
    Light__string_Green Light__string = 1
)

type GoError = error

func show_int(x__1 Light__int32) string {
    var retv3 string
    var jp5 string
    switch x__1 {
    case Light__int32_Red:
        jp5 = "ri"
    case Light__int32_Green:
        jp5 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv3 = jp5
    return retv3
}

func show_string(x__2 Light__string) string {
    var retv7 string
    var jp9 string
    switch x__2 {
    case Light__string_Red:
        jp9 = "rs"
    case Light__string_Green:
        jp9 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv7 = jp9
    return retv7
}

func main0() struct{} {
    var t11 Light__int32 = flip__T_int32(Light__int32_Red)
    var t12 string = show_int(t11)
    println__T_string(t12)
    var t13 Light__string = flip__T_string(Light__string_Green)
    var t14 string = show_string(t13)
    println__T_string(t14)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv18 Light__int32
    var jp20 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp20 = Light__int32_Green
    case Light__int32_Green:
        jp20 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv22 Light__string
    var jp24 Light__string
    switch x__0 {
    case Light__string_Red:
        jp24 = Light__string_Green
    case Light__string_Green:
        jp24 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main() {
    main0()
}
