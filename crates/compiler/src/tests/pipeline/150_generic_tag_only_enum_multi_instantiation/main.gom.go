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
    var retv61 string
    var jp63 string
    switch x__1 {
    case Light__int32_Red:
        jp63 = "ri"
    case Light__int32_Green:
        jp63 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv61 = jp63
    return retv61
}

func show_string(x__2 Light__string) string {
    var retv65 string
    var jp67 string
    switch x__2 {
    case Light__string_Red:
        jp67 = "rs"
    case Light__string_Green:
        jp67 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv65 = jp67
    return retv65
}

func main0() struct{} {
    var t69 Light__int32 = flip__T_int32(Light__int32_Red)
    var t70 string = show_int(t69)
    println__T_string(t70)
    var t71 Light__string = flip__T_string(Light__string_Green)
    var t72 string = show_string(t71)
    println__T_string(t72)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv77 Light__int32
    var jp79 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp79 = Light__int32_Green
    case Light__int32_Green:
        jp79 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv77 = jp79
    return retv77
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv81 Light__string
    var jp83 Light__string
    switch x__0 {
    case Light__string_Red:
        jp83 = Light__string_Green
    case Light__string_Green:
        jp83 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv85 string
    retv85 = self__34
    return retv85
}

func main() {
    main0()
}
