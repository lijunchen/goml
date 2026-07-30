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
    var retv71 string
    var jp73 string
    switch x__1 {
    case Light__int32_Red:
        jp73 = "ri"
    case Light__int32_Green:
        jp73 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv71 = jp73
    return retv71
}

func show_string(x__2 Light__string) string {
    var retv75 string
    var jp77 string
    switch x__2 {
    case Light__string_Red:
        jp77 = "rs"
    case Light__string_Green:
        jp77 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv75 = jp77
    return retv75
}

func main0() struct{} {
    var t79 Light__int32 = flip__T_int32(Light__int32_Red)
    var t80 string = show_int(t79)
    println__T_string(t80)
    var t81 Light__string = flip__T_string(Light__string_Green)
    var t82 string = show_string(t81)
    println__T_string(t82)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv87 Light__int32
    var jp89 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp89 = Light__int32_Green
    case Light__int32_Green:
        jp89 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv91 Light__string
    var jp93 Light__string
    switch x__0 {
    case Light__string_Red:
        jp93 = Light__string_Green
    case Light__string_Green:
        jp93 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv91 = jp93
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv95 string
    retv95 = self__38
    return retv95
}

func main() {
    main0()
}
