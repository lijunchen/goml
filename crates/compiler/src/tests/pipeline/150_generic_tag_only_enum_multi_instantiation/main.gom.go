package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
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

func show_int(x__1 Light__int32) string {
    var retv64 string
    var jp66 string
    switch x__1 {
    case Light__int32_Red:
        jp66 = "ri"
    case Light__int32_Green:
        jp66 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv64 = jp66
    return retv64
}

func show_string(x__2 Light__string) string {
    var retv68 string
    var jp70 string
    switch x__2 {
    case Light__string_Red:
        jp70 = "rs"
    case Light__string_Green:
        jp70 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv68 = jp70
    return retv68
}

func main0() struct{} {
    var t72 Light__int32 = flip__T_int32(Light__int32_Red)
    var t73 string = show_int(t72)
    println__T_string(t73)
    var t74 Light__string = flip__T_string(Light__string_Green)
    var t75 string = show_string(t74)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv80 Light__int32
    var jp82 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp82 = Light__int32_Green
    case Light__int32_Green:
        jp82 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv80 = jp82
    return retv80
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv84 Light__string
    var jp86 Light__string
    switch x__0 {
    case Light__string_Red:
        jp86 = Light__string_Green
    case Light__string_Green:
        jp86 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv88 string
    retv88 = self__37
    return retv88
}

func main() {
    main0()
}
