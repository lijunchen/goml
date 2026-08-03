package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t187 string
    var inline221 float64 = 0
    switch inline221 {
    case 0:
        t187 = "zero"
    case 1:
        t187 = "one"
    case -1:
        t187 = "minus one"
    case 3.14:
        t187 = "pi"
    default:
        t187 = "other"
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline218)
    var t188 string
    var inline216 float64 = 1
    switch inline216 {
    case 0:
        t188 = "zero"
    case 1:
        t188 = "one"
    case -1:
        t188 = "minus one"
    case 3.14:
        t188 = "pi"
    default:
        t188 = "other"
    }
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline213)
    var t189 float64 = -1
    var t190 string
    switch t189 {
    case 0:
        t190 = "zero"
    case 1:
        t190 = "one"
    case -1:
        t190 = "minus one"
    case 3.14:
        t190 = "pi"
    default:
        t190 = "other"
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline209)
    var t191 string
    var inline207 float64 = 3.14
    switch inline207 {
    case 0:
        t191 = "zero"
    case 1:
        t191 = "one"
    case -1:
        t191 = "minus one"
    case 3.14:
        t191 = "pi"
    default:
        t191 = "other"
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline204)
    var t192 string
    var inline202 float64 = 42
    switch inline202 {
    case 0:
        t192 = "zero"
    case 1:
        t192 = "one"
    case -1:
        t192 = "minus one"
    case 3.14:
        t192 = "pi"
    default:
        t192 = "other"
    }
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
