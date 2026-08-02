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
    var t166 Light__int32
    t166 = Light__int32_Green
    var t167 string
    switch t166 {
    case Light__int32_Red:
        t167 = "ri"
    case Light__int32_Green:
        t167 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline189)
    var t168 Light__string
    t168 = Light__string_Red
    var t169 string
    switch t168 {
    case Light__string_Red:
        t169 = "rs"
    case Light__string_Green:
        t169 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
