package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t191 string
    var inline219 int32 = -1
    switch inline219 {
    case -1:
        t191 = "minus one"
    case 0:
        t191 = "zero"
    case 1:
        t191 = "one"
    default:
        t191 = "other"
    }
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline216)
    var t192 string
    var inline214 int32 = 0
    switch inline214 {
    case -1:
        t192 = "minus one"
    case 0:
        t192 = "zero"
    case 1:
        t192 = "one"
    default:
        t192 = "other"
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline211)
    var t193 string
    var inline209 int32 = 1
    switch inline209 {
    case -1:
        t193 = "minus one"
    case 0:
        t193 = "zero"
    case 1:
        t193 = "one"
    default:
        t193 = "other"
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline206)
    var t194 string
    var inline204 int32 = 42
    switch inline204 {
    case -1:
        t194 = "minus one"
    case 0:
        t194 = "zero"
    case 1:
        t194 = "one"
    default:
        t194 = "other"
    }
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
