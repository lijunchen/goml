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
    switch x__1 {
    case Light__int32_Red:
        return "ri"
    case Light__int32_Green:
        return "gi"
    default:
        panic("non-exhaustive match")
    }
}

func show_string(x__2 Light__string) string {
    switch x__2 {
    case Light__string_Red:
        return "rs"
    case Light__string_Green:
        return "gs"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t166 Light__int32 = flip__T_int32(Light__int32_Red)
    var t167 string = show_int(t166)
    println__T_string(t167)
    var t168 Light__string = flip__T_string(Light__string_Green)
    var t169 string = show_string(t168)
    println__T_string(t169)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    switch x__0 {
    case Light__int32_Red:
        return Light__int32_Green
    case Light__int32_Green:
        return Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
}

func flip__T_string(x__0 Light__string) Light__string {
    switch x__0 {
    case Light__string_Red:
        return Light__string_Green
    case Light__string_Green:
        return Light__string_Red
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
