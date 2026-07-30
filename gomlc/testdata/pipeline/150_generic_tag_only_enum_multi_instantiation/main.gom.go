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
    var retv111 string
    var jp113 string
    switch x__1 {
    case Light__int32_Red:
        jp113 = "ri"
    case Light__int32_Green:
        jp113 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv111 = jp113
    return retv111
}

func show_string(x__2 Light__string) string {
    var retv115 string
    var jp117 string
    switch x__2 {
    case Light__string_Red:
        jp117 = "rs"
    case Light__string_Green:
        jp117 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv115 = jp117
    return retv115
}

func main0() struct{} {
    var t119 Light__int32 = flip__T_int32(Light__int32_Red)
    var t120 string = show_int(t119)
    println__T_string(t120)
    var t121 Light__string = flip__T_string(Light__string_Green)
    var t122 string = show_string(t121)
    println__T_string(t122)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t124 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t124)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv127 Light__int32
    var jp129 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp129 = Light__int32_Green
    case Light__int32_Green:
        jp129 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv127 = jp129
    return retv127
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv131 Light__string
    var jp133 Light__string
    switch x__0 {
    case Light__string_Red:
        jp133 = Light__string_Green
    case Light__string_Green:
        jp133 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv131 = jp133
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv135 string
    retv135 = self__38
    return retv135
}

func main() {
    main0()
}
