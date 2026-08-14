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

func match_int(n__0 int32) int32 {
    switch n__0 {
    case 0:
        return 10
    case 1:
        return 20
    default:
        return 30
    }
}

func main0() struct{} {
    var t430 int32 = match_int(0)
    var t431 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t430)
    println__T_string(t431)
    var t432 int32 = match_int(5)
    var t433 string
    var inline498 string = _goml_runtime_core_int32_to_string(t432)
    t433 = inline498
    println__T_string(t433)
    var t434 int32
    t434 = 40
    var t435 string
    var inline494 string = _goml_runtime_core_int32_to_string(t434)
    t435 = inline494
    var inline491 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline491)
    var t436 int32
    t436 = 40
    var t437 string
    var inline487 string = _goml_runtime_core_int32_to_string(t436)
    t437 = inline487
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline484)
    var t438 int32
    var inline482 int32 = 2
    switch inline482 {
    case 2:
        t438 = 90
    case 3:
        t438 = 100
    default:
        t438 = 100
    }
    var t439 string
    var inline480 string = _goml_runtime_core_int32_to_string(t438)
    t439 = inline480
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline477)
    var t440 int32
    var inline475 int32 = 3
    switch inline475 {
    case 2:
        t440 = 90
    case 3:
        t440 = 100
    default:
        t440 = 100
    }
    var t441 string
    var inline473 string = _goml_runtime_core_int32_to_string(t440)
    t441 = inline473
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline470)
    var t442 int32
    var inline468 int32 = 1
    switch inline468 {
    case 1:
        t442 = 60
    default:
        t442 = 80
    }
    var t443 string
    var inline466 string = _goml_runtime_core_int32_to_string(t442)
    t443 = inline466
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline463)
    var t444 int32
    var inline461 int32 = 3
    switch inline461 {
    case 1:
        t444 = 60
    default:
        t444 = 80
    }
    var t445 string
    var inline459 string = _goml_runtime_core_int32_to_string(t444)
    t445 = inline459
    var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline456)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t448 string
    t448 = value__1
    _goml_runtime_core_string_println(t448)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t452 string = _goml_runtime_core_int32_to_string(self__33)
    return t452
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
