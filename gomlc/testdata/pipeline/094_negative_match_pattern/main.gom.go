package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t196 string
    var inline224 int32 = -1
    switch inline224 {
    case -1:
        t196 = "minus one"
    case 0:
        t196 = "zero"
    case 1:
        t196 = "one"
    default:
        t196 = "other"
    }
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline221)
    var t197 string
    var inline219 int32 = 0
    switch inline219 {
    case -1:
        t197 = "minus one"
    case 0:
        t197 = "zero"
    case 1:
        t197 = "one"
    default:
        t197 = "other"
    }
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline216)
    var t198 string
    var inline214 int32 = 1
    switch inline214 {
    case -1:
        t198 = "minus one"
    case 0:
        t198 = "zero"
    case 1:
        t198 = "one"
    default:
        t198 = "other"
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline211)
    var t199 string
    var inline209 int32 = 42
    switch inline209 {
    case -1:
        t199 = "minus one"
    case 0:
        t199 = "zero"
    case 1:
        t199 = "one"
    default:
        t199 = "other"
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
