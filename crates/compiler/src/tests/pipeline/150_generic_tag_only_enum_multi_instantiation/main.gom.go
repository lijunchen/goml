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
    var retv7 string
    var jp9 string
    switch x__1 {
    case Light__int32_Red:
        jp9 = "ri"
    case Light__int32_Green:
        jp9 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv7 = jp9
    return retv7
}

func show_string(x__2 Light__string) string {
    var retv11 string
    var jp13 string
    switch x__2 {
    case Light__string_Red:
        jp13 = "rs"
    case Light__string_Green:
        jp13 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func main0() struct{} {
    var t15 Light__int32 = flip__T_int32(Light__int32_Red)
    var t16 string = show_int(t15)
    println__T_string(t16)
    var t17 Light__string = flip__T_string(Light__string_Green)
    var t18 string = show_string(t17)
    println__T_string(t18)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t20)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv23 Light__int32
    var jp25 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp25 = Light__int32_Green
    case Light__int32_Green:
        jp25 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv23 = jp25
    return retv23
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv27 Light__string
    var jp29 Light__string
    switch x__0 {
    case Light__string_Red:
        jp29 = Light__string_Green
    case Light__string_Green:
        jp29 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv31 string
    retv31 = self__9
    return retv31
}

func main() {
    main0()
}
