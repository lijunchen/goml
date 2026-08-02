package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t164 string
    var inline192 int32 = -1
    switch inline192 {
    case -1:
        t164 = "minus one"
    case 0:
        t164 = "zero"
    case 1:
        t164 = "one"
    default:
        t164 = "other"
    }
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline189)
    var t165 string
    var inline187 int32 = 0
    switch inline187 {
    case -1:
        t165 = "minus one"
    case 0:
        t165 = "zero"
    case 1:
        t165 = "one"
    default:
        t165 = "other"
    }
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline184)
    var t166 string
    var inline182 int32 = 1
    switch inline182 {
    case -1:
        t166 = "minus one"
    case 0:
        t166 = "zero"
    case 1:
        t166 = "one"
    default:
        t166 = "other"
    }
    var inline179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline179)
    var t167 string
    var inline177 int32 = 42
    switch inline177 {
    case -1:
        t167 = "minus one"
    case 0:
        t167 = "zero"
    case 1:
        t167 = "one"
    default:
        t167 = "other"
    }
    var inline174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline174)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
