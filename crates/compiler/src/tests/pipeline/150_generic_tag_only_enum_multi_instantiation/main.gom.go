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
    var retv25 string
    var jp27 string
    switch x__1 {
    case Light__int32_Red:
        jp27 = "ri"
    case Light__int32_Green:
        jp27 = "gi"
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func show_string(x__2 Light__string) string {
    var retv29 string
    var jp31 string
    switch x__2 {
    case Light__string_Red:
        jp31 = "rs"
    case Light__string_Green:
        jp31 = "gs"
    default:
        panic("non-exhaustive match")
    }
    retv29 = jp31
    return retv29
}

func main0() struct{} {
    var t33 Light__int32 = flip__T_int32(Light__int32_Red)
    var t34 string = show_int(t33)
    println__T_string(t34)
    var t35 Light__string = flip__T_string(Light__string_Green)
    var t36 string = show_string(t35)
    println__T_string(t36)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func flip__T_int32(x__0 Light__int32) Light__int32 {
    var retv41 Light__int32
    var jp43 Light__int32
    switch x__0 {
    case Light__int32_Red:
        jp43 = Light__int32_Green
    case Light__int32_Green:
        jp43 = Light__int32_Red
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func flip__T_string(x__0 Light__string) Light__string {
    var retv45 Light__string
    var jp47 Light__string
    switch x__0 {
    case Light__string_Red:
        jp47 = Light__string_Green
    case Light__string_Green:
        jp47 = Light__string_Red
    default:
        panic("non-exhaustive match")
    }
    retv45 = jp47
    return retv45
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv49 string
    retv49 = self__9
    return retv49
}

func main() {
    main0()
}
