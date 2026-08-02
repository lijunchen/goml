package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t165 string
    var inline199 float64 = 0
    switch inline199 {
    case 0:
        t165 = "zero"
    case 1:
        t165 = "one"
    case -1:
        t165 = "minus one"
    case 3.14:
        t165 = "pi"
    default:
        t165 = "other"
    }
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline196)
    var t166 string
    var inline194 float64 = 1
    switch inline194 {
    case 0:
        t166 = "zero"
    case 1:
        t166 = "one"
    case -1:
        t166 = "minus one"
    case 3.14:
        t166 = "pi"
    default:
        t166 = "other"
    }
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline191)
    var t167 float64 = -1
    var t168 string
    switch t167 {
    case 0:
        t168 = "zero"
    case 1:
        t168 = "one"
    case -1:
        t168 = "minus one"
    case 3.14:
        t168 = "pi"
    default:
        t168 = "other"
    }
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t168)
    _goml_runtime_core_string_println(inline187)
    var t169 string
    var inline185 float64 = 3.14
    switch inline185 {
    case 0:
        t169 = "zero"
    case 1:
        t169 = "one"
    case -1:
        t169 = "minus one"
    case 3.14:
        t169 = "pi"
    default:
        t169 = "other"
    }
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline182)
    var t170 string
    var inline180 float64 = 42
    switch inline180 {
    case 0:
        t170 = "zero"
    case 1:
        t170 = "one"
    case -1:
        t170 = "minus one"
    case 3.14:
        t170 = "pi"
    default:
        t170 = "other"
    }
    var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline177)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
