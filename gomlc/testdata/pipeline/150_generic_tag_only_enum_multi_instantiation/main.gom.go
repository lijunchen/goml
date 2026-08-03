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
    var t147 Light__int32
    t147 = Light__int32_Green
    var t148 string
    switch t147 {
    case Light__int32_Red:
        t148 = "ri"
    case Light__int32_Green:
        t148 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline170)
    var t149 Light__string
    t149 = Light__string_Red
    var t150 string
    switch t149 {
    case Light__string_Red:
        t150 = "rs"
    case Light__string_Green:
        t150 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
