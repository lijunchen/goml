package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t197 string
    var inline231 float64 = 0
    switch inline231 {
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
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline228)
    var t198 string
    var inline226 float64 = 1
    switch inline226 {
    case 0:
        t198 = "zero"
    case 1:
        t198 = "one"
    case -1:
        t198 = "minus one"
    case 3.14:
        t198 = "pi"
    default:
        t198 = "other"
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline223)
    var t199 float64 = -1
    var t200 string
    switch t199 {
    case 0:
        t200 = "zero"
    case 1:
        t200 = "one"
    case -1:
        t200 = "minus one"
    case 3.14:
        t200 = "pi"
    default:
        t200 = "other"
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline219)
    var t201 string
    var inline217 float64 = 3.14
    switch inline217 {
    case 0:
        t201 = "zero"
    case 1:
        t201 = "one"
    case -1:
        t201 = "minus one"
    case 3.14:
        t201 = "pi"
    default:
        t201 = "other"
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline214)
    var t202 string
    var inline212 float64 = 42
    switch inline212 {
    case 0:
        t202 = "zero"
    case 1:
        t202 = "one"
    case -1:
        t202 = "minus one"
    case 3.14:
        t202 = "pi"
    default:
        t202 = "other"
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
