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
    var retv155 string
    var jp157 string
    switch x__1 {
    case Light__int32_Red:
        jp157 = "ri"
    case Light__int32_Green:
        jp157 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv155 = jp157
    return retv155
}

func show_string(x__2 Light__string) string {
    var retv159 string
    var jp161 string
    switch x__2 {
    case Light__string_Red:
        jp161 = "rs"
    case Light__string_Green:
        jp161 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func main0() struct{} {
    var t163 Light__int32 = flip__T_int32(Light__int32_Red)
    var t164 string = show_int(t163)
    println__T_string(t164)
    var t165 Light__string = flip__T_string(Light__string_Green)
    var t166 string = show_string(t165)
    println__T_string(t166)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv171 Light__int32
    var jp173 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp173 = Light__int32_Green
    case Light__int32_Green:
        jp173 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv171 = jp173
    return retv171
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv175 Light__string
    var jp177 Light__string
    switch x__0 {
    case Light__string_Red:
        jp177 = Light__string_Green
    case Light__string_Green:
        jp177 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv179 string
    retv179 = self__38
    return retv179
}

func main() {
    main0()
}
