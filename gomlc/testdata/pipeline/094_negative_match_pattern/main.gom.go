package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t186 string
    var inline214 int32 = -1
    switch inline214 {
    case -1:
        t186 = "minus one"
    case 0:
        t186 = "zero"
    case 1:
        t186 = "one"
    default:
        t186 = "other"
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline211)
    var t187 string
    var inline209 int32 = 0
    switch inline209 {
    case -1:
        t187 = "minus one"
    case 0:
        t187 = "zero"
    case 1:
        t187 = "one"
    default:
        t187 = "other"
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline206)
    var t188 string
    var inline204 int32 = 1
    switch inline204 {
    case -1:
        t188 = "minus one"
    case 0:
        t188 = "zero"
    case 1:
        t188 = "one"
    default:
        t188 = "other"
    }
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline201)
    var t189 string
    var inline199 int32 = 42
    switch inline199 {
    case -1:
        t189 = "minus one"
    case 0:
        t189 = "zero"
    case 1:
        t189 = "one"
    default:
        t189 = "other"
    }
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
