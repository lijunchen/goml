package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t146 string
    var inline180 float64 = 0
    switch inline180 {
    case 0:
        t146 = "zero"
    case 1:
        t146 = "one"
    case -1:
        t146 = "minus one"
    case 3.14:
        t146 = "pi"
    default:
        t146 = "other"
    }
    var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline177)
    var t147 string
    var inline175 float64 = 1
    switch inline175 {
    case 0:
        t147 = "zero"
    case 1:
        t147 = "one"
    case -1:
        t147 = "minus one"
    case 3.14:
        t147 = "pi"
    default:
        t147 = "other"
    }
    var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline172)
    var t148 float64 = -1
    var t149 string
    switch t148 {
    case 0:
        t149 = "zero"
    case 1:
        t149 = "one"
    case -1:
        t149 = "minus one"
    case 3.14:
        t149 = "pi"
    default:
        t149 = "other"
    }
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t149)
    _goml_runtime_core_string_println(inline168)
    var t150 string
    var inline166 float64 = 3.14
    switch inline166 {
    case 0:
        t150 = "zero"
    case 1:
        t150 = "one"
    case -1:
        t150 = "minus one"
    case 3.14:
        t150 = "pi"
    default:
        t150 = "other"
    }
    var inline163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline163)
    var t151 string
    var inline161 float64 = 42
    switch inline161 {
    case 0:
        t151 = "zero"
    case 1:
        t151 = "one"
    case -1:
        t151 = "minus one"
    case 3.14:
        t151 = "pi"
    default:
        t151 = "other"
    }
    var inline158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline158)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
