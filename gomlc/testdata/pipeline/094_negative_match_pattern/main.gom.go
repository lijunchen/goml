package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t181 string
    var inline209 int32 = -1
    switch inline209 {
    case -1:
        t181 = "minus one"
    case 0:
        t181 = "zero"
    case 1:
        t181 = "one"
    default:
        t181 = "other"
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline206)
    var t182 string
    var inline204 int32 = 0
    switch inline204 {
    case -1:
        t182 = "minus one"
    case 0:
        t182 = "zero"
    case 1:
        t182 = "one"
    default:
        t182 = "other"
    }
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline201)
    var t183 string
    var inline199 int32 = 1
    switch inline199 {
    case -1:
        t183 = "minus one"
    case 0:
        t183 = "zero"
    case 1:
        t183 = "one"
    default:
        t183 = "other"
    }
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline196)
    var t184 string
    var inline194 int32 = 42
    switch inline194 {
    case -1:
        t184 = "minus one"
    case 0:
        t184 = "zero"
    case 1:
        t184 = "one"
    default:
        t184 = "other"
    }
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
