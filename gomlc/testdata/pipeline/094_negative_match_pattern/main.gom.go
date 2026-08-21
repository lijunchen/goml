package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var t420 string
    var inline448 int32 = -1
    switch inline448 {
    case -1:
        t420 = "minus one"
    case 0:
        t420 = "zero"
    case 1:
        t420 = "one"
    default:
        t420 = "other"
    }
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline445)
    var t421 string
    var inline443 int32 = 0
    switch inline443 {
    case -1:
        t421 = "minus one"
    case 0:
        t421 = "zero"
    case 1:
        t421 = "one"
    default:
        t421 = "other"
    }
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline440)
    var t422 string
    var inline438 int32 = 1
    switch inline438 {
    case -1:
        t422 = "minus one"
    case 0:
        t422 = "zero"
    case 1:
        t422 = "one"
    default:
        t422 = "other"
    }
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline435)
    var t423 string
    var inline433 int32 = 42
    switch inline433 {
    case -1:
        t423 = "minus one"
    case 0:
        t423 = "zero"
    case 1:
        t423 = "one"
    default:
        t423 = "other"
    }
    var inline430 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline430)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
