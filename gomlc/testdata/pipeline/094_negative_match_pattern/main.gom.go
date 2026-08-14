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
    var t417 string
    var inline445 int32 = -1
    switch inline445 {
    case -1:
        t417 = "minus one"
    case 0:
        t417 = "zero"
    case 1:
        t417 = "one"
    default:
        t417 = "other"
    }
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline442)
    var t418 string
    var inline440 int32 = 0
    switch inline440 {
    case -1:
        t418 = "minus one"
    case 0:
        t418 = "zero"
    case 1:
        t418 = "one"
    default:
        t418 = "other"
    }
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline437)
    var t419 string
    var inline435 int32 = 1
    switch inline435 {
    case -1:
        t419 = "minus one"
    case 0:
        t419 = "zero"
    case 1:
        t419 = "one"
    default:
        t419 = "other"
    }
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline432)
    var t420 string
    var inline430 int32 = 42
    switch inline430 {
    case -1:
        t420 = "minus one"
    case 0:
        t420 = "zero"
    case 1:
        t420 = "one"
    default:
        t420 = "other"
    }
    var inline427 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline427)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
