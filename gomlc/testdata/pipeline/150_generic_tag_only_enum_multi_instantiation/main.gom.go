package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

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
    var t419 Light__int32
    t419 = Light__int32_Green
    var t420 string
    switch t419 {
    case Light__int32_Red:
        t420 = "ri"
    case Light__int32_Green:
        t420 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline442)
    var t421 Light__string
    t421 = Light__string_Red
    var t422 string
    switch t421 {
    case Light__string_Red:
        t422 = "rs"
    case Light__string_Green:
        t422 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
