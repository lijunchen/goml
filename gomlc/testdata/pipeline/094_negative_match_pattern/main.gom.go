package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t145 string
    var inline173 int32 = -1
    switch inline173 {
    case -1:
        t145 = "minus one"
    case 0:
        t145 = "zero"
    case 1:
        t145 = "one"
    default:
        t145 = "other"
    }
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline170)
    var t146 string
    var inline168 int32 = 0
    switch inline168 {
    case -1:
        t146 = "minus one"
    case 0:
        t146 = "zero"
    case 1:
        t146 = "one"
    default:
        t146 = "other"
    }
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline165)
    var t147 string
    var inline163 int32 = 1
    switch inline163 {
    case -1:
        t147 = "minus one"
    case 0:
        t147 = "zero"
    case 1:
        t147 = "one"
    default:
        t147 = "other"
    }
    var inline160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline160)
    var t148 string
    var inline158 int32 = 42
    switch inline158 {
    case -1:
        t148 = "minus one"
    case 0:
        t148 = "zero"
    case 1:
        t148 = "one"
    default:
        t148 = "other"
    }
    var inline155 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline155)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
