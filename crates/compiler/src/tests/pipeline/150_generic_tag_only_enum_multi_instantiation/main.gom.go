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
    var retv10 string
    var jp12 string
    switch x__1 {
    case Light__int32_Red:
        jp12 = "ri"
    case Light__int32_Green:
        jp12 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv10 = jp12
    return retv10
}

func show_string(x__2 Light__string) string {
    var retv14 string
    var jp16 string
    switch x__2 {
    case Light__string_Red:
        jp16 = "rs"
    case Light__string_Green:
        jp16 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv14 = jp16
    return retv14
}

func main0() struct{} {
    var t18 Light__int32 = flip__T_int32(Light__int32_Red)
    var t19 string = show_int(t18)
    println__T_string(t19)
    var t20 Light__string = flip__T_string(Light__string_Green)
    var t21 string = show_string(t20)
    println__T_string(t21)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t23 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t23)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv26 Light__int32
    var jp28 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp28 = Light__int32_Green
    case Light__int32_Green:
        jp28 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv30 Light__string
    var jp32 Light__string
    switch x__0 {
    case Light__string_Red:
        jp32 = Light__string_Green
    case Light__string_Green:
        jp32 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv34 string
    retv34 = self__9
    return retv34
}

func main() {
    main0()
}
