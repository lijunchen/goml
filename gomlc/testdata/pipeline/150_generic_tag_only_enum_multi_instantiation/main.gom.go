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
    var t198 Light__int32
    t198 = Light__int32_Green
    var t199 string
    switch t198 {
    case Light__int32_Red:
        t199 = "ri"
    case Light__int32_Green:
        t199 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline221)
    var t200 Light__string
    t200 = Light__string_Red
    var t201 string
    switch t200 {
    case Light__string_Red:
        t201 = "rs"
    case Light__string_Green:
        t201 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
