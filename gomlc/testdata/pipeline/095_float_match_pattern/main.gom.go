package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t192 string
    var inline226 float64 = 0
    switch inline226 {
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
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline223)
    var t193 string
    var inline221 float64 = 1
    switch inline221 {
    case 0:
        t193 = "zero"
    case 1:
        t193 = "one"
    case -1:
        t193 = "minus one"
    case 3.14:
        t193 = "pi"
    default:
        t193 = "other"
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline218)
    var t194 float64 = -1
    var t195 string
    switch t194 {
    case 0:
        t195 = "zero"
    case 1:
        t195 = "one"
    case -1:
        t195 = "minus one"
    case 3.14:
        t195 = "pi"
    default:
        t195 = "other"
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline214)
    var t196 string
    var inline212 float64 = 3.14
    switch inline212 {
    case 0:
        t196 = "zero"
    case 1:
        t196 = "one"
    case -1:
        t196 = "minus one"
    case 3.14:
        t196 = "pi"
    default:
        t196 = "other"
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline209)
    var t197 string
    var inline207 float64 = 42
    switch inline207 {
    case 0:
        t197 = "zero"
    case 1:
        t197 = "one"
    case -1:
        t197 = "minus one"
    case 3.14:
        t197 = "pi"
    default:
        t197 = "other"
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
