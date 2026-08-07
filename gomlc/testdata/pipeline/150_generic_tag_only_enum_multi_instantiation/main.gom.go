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
    var t183 Light__int32
    t183 = Light__int32_Green
    var t184 string
    switch t183 {
    case Light__int32_Red:
        t184 = "ri"
    case Light__int32_Green:
        t184 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline206)
    var t185 Light__string
    t185 = Light__string_Red
    var t186 string
    switch t185 {
    case Light__string_Red:
        t186 = "rs"
    case Light__string_Green:
        t186 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
