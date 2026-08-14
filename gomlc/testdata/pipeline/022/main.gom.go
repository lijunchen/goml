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
    var t424 int32
    var inline486 string = "hello"
    switch inline486 {
    case "hello":
        t424 = 1
    case "world":
        t424 = 2
    default:
        t424 = 3
    }
    var t425 string
    var inline484 string = _goml_runtime_core_int32_to_string(t424)
    t425 = inline484
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline481)
    var t426 int32
    var inline479 string = "planet"
    switch inline479 {
    case "hello":
        t426 = 1
    case "world":
        t426 = 2
    default:
        t426 = 3
    }
    var t427 string
    var inline477 string = _goml_runtime_core_int32_to_string(t426)
    t427 = inline477
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline474)
    var t428 int32
    t428 = 4
    var t429 string
    var inline470 string = _goml_runtime_core_int32_to_string(t428)
    t429 = inline470
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline467)
    var t430 int32
    t430 = 4
    var t431 string
    var inline463 string = _goml_runtime_core_int32_to_string(t430)
    t431 = inline463
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline460)
    var t432 int32
    var inline458 string = "hello"
    switch inline458 {
    case "hello":
        t432 = 6
    default:
        t432 = 8
    }
    var t433 string
    var inline456 string = _goml_runtime_core_int32_to_string(t432)
    t433 = inline456
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline453)
    var t434 int32
    var inline451 string = "mars"
    switch inline451 {
    case "hello":
        t434 = 6
    default:
        t434 = 8
    }
    var t435 string
    var inline449 string = _goml_runtime_core_int32_to_string(t434)
    t435 = inline449
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline446)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
