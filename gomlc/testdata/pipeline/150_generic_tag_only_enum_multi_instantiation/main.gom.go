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

func main0() struct{} {
    var t188 Light__int32
    t188 = Light__int32_Green
    var t189 string
    switch t188 {
    case Light__int32_Red:
        t189 = "ri"
    case Light__int32_Green:
        t189 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline211)
    var t190 Light__string
    t190 = Light__string_Red
    var t191 string
    switch t190 {
    case Light__string_Red:
        t191 = "rs"
    case Light__string_Green:
        t191 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
