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
    var t418 string
    var inline452 float64 = 0
    switch inline452 {
    case 0:
        t418 = "zero"
    case 1:
        t418 = "one"
    case -1:
        t418 = "minus one"
    case 3.14:
        t418 = "pi"
    default:
        t418 = "other"
    }
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline449)
    var t419 string
    var inline447 float64 = 1
    switch inline447 {
    case 0:
        t419 = "zero"
    case 1:
        t419 = "one"
    case -1:
        t419 = "minus one"
    case 3.14:
        t419 = "pi"
    default:
        t419 = "other"
    }
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline444)
    var t420 float64 = -1
    var t421 string
    switch t420 {
    case 0:
        t421 = "zero"
    case 1:
        t421 = "one"
    case -1:
        t421 = "minus one"
    case 3.14:
        t421 = "pi"
    default:
        t421 = "other"
    }
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline440)
    var t422 string
    var inline438 float64 = 3.14
    switch inline438 {
    case 0:
        t422 = "zero"
    case 1:
        t422 = "one"
    case -1:
        t422 = "minus one"
    case 3.14:
        t422 = "pi"
    default:
        t422 = "other"
    }
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline435)
    var t423 string
    var inline433 float64 = 42
    switch inline433 {
    case 0:
        t423 = "zero"
    case 1:
        t423 = "one"
    case -1:
        t423 = "minus one"
    case 3.14:
        t423 = "pi"
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
