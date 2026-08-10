package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t182 string
    var inline216 float64 = 0
    switch inline216 {
    case 0:
        t182 = "zero"
    case 1:
        t182 = "one"
    case -1:
        t182 = "minus one"
    case 3.14:
        t182 = "pi"
    default:
        t182 = "other"
    }
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline213)
    var t183 string
    var inline211 float64 = 1
    switch inline211 {
    case 0:
        t183 = "zero"
    case 1:
        t183 = "one"
    case -1:
        t183 = "minus one"
    case 3.14:
        t183 = "pi"
    default:
        t183 = "other"
    }
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline208)
    var t184 float64 = -1
    var t185 string
    switch t184 {
    case 0:
        t185 = "zero"
    case 1:
        t185 = "one"
    case -1:
        t185 = "minus one"
    case 3.14:
        t185 = "pi"
    default:
        t185 = "other"
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline204)
    var t186 string
    var inline202 float64 = 3.14
    switch inline202 {
    case 0:
        t186 = "zero"
    case 1:
        t186 = "one"
    case -1:
        t186 = "minus one"
    case 3.14:
        t186 = "pi"
    default:
        t186 = "other"
    }
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline199)
    var t187 string
    var inline197 float64 = 42
    switch inline197 {
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
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
