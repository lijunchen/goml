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
    var t422 Light__int32
    t422 = Light__int32_Green
    var t423 string
    switch t422 {
    case Light__int32_Red:
        t423 = "ri"
    case Light__int32_Green:
        t423 = "gi"
    default:
        panic("non-exhaustive match")
    }
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline445)
    var t424 Light__string
    t424 = Light__string_Red
    var t425 string
    switch t424 {
    case Light__string_Red:
        t425 = "rs"
    case Light__string_Green:
        t425 = "gs"
    default:
        panic("non-exhaustive match")
    }
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
