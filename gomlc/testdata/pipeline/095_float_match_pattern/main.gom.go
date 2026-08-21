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
    var t421 string
    var inline455 float64 = 0
    switch inline455 {
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
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline452)
    var t422 string
    var inline450 float64 = 1
    switch inline450 {
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
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline447)
    var t423 float64 = -1
    var t424 string
    switch t423 {
    case 0:
        t424 = "zero"
    case 1:
        t424 = "one"
    case -1:
        t424 = "minus one"
    case 3.14:
        t424 = "pi"
    default:
        t424 = "other"
    }
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline443)
    var t425 string
    var inline441 float64 = 3.14
    switch inline441 {
    case 0:
        t425 = "zero"
    case 1:
        t425 = "one"
    case -1:
        t425 = "minus one"
    case 3.14:
        t425 = "pi"
    default:
        t425 = "other"
    }
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline438)
    var t426 string
    var inline436 float64 = 42
    switch inline436 {
    case 0:
        t426 = "zero"
    case 1:
        t426 = "one"
    case -1:
        t426 = "minus one"
    case 3.14:
        t426 = "pi"
    default:
        t426 = "other"
    }
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline433)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
