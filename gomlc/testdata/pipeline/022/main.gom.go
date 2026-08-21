package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var t427 int32
    var inline489 string = "hello"
    switch inline489 {
    case "hello":
        t427 = 1
    case "world":
        t427 = 2
    default:
        t427 = 3
    }
    var t428 string
    var inline487 string = _goml_runtime_core_int32_to_string(t427)
    t428 = inline487
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline484)
    var t429 int32
    var inline482 string = "planet"
    switch inline482 {
    case "hello":
        t429 = 1
    case "world":
        t429 = 2
    default:
        t429 = 3
    }
    var t430 string
    var inline480 string = _goml_runtime_core_int32_to_string(t429)
    t430 = inline480
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline477)
    var t431 int32
    t431 = 4
    var t432 string
    var inline473 string = _goml_runtime_core_int32_to_string(t431)
    t432 = inline473
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline470)
    var t433 int32
    t433 = 4
    var t434 string
    var inline466 string = _goml_runtime_core_int32_to_string(t433)
    t434 = inline466
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline463)
    var t435 int32
    var inline461 string = "hello"
    switch inline461 {
    case "hello":
        t435 = 6
    default:
        t435 = 8
    }
    var t436 string
    var inline459 string = _goml_runtime_core_int32_to_string(t435)
    t436 = inline459
    var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline456)
    var t437 int32
    var inline454 string = "mars"
    switch inline454 {
    case "hello":
        t437 = 6
    default:
        t437 = 8
    }
    var t438 string
    var inline452 string = _goml_runtime_core_int32_to_string(t437)
    t438 = inline452
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline449)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
