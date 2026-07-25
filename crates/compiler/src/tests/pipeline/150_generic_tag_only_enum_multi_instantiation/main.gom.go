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
    var retv67 string
    var jp69 string
    switch x__1 {
    case Light__int32_Red:
        jp69 = "ri"
    case Light__int32_Green:
        jp69 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv67 = jp69
    return retv67
}

func show_string(x__2 Light__string) string {
    var retv71 string
    var jp73 string
    switch x__2 {
    case Light__string_Red:
        jp73 = "rs"
    case Light__string_Green:
        jp73 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv71 = jp73
    return retv71
}

func main0() struct{} {
    var t75 Light__int32 = flip__T_int32(Light__int32_Red)
    var t76 string = show_int(t75)
    println__T_string(t76)
    var t77 Light__string = flip__T_string(Light__string_Green)
    var t78 string = show_string(t77)
    println__T_string(t78)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv83 Light__int32
    var jp85 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp85 = Light__int32_Green
    case Light__int32_Green:
        jp85 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv83 = jp85
    return retv83
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv87 Light__string
    var jp89 Light__string
    switch x__0 {
    case Light__string_Red:
        jp89 = Light__string_Green
    case Light__string_Green:
        jp89 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv91 string
    retv91 = self__38
    return retv91
}

func main() {
    main0()
}
