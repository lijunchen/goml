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
    var t433 int32 = match_int(0)
    var t434 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t433)
    println__T_string(t434)
    var t435 int32 = match_int(5)
    var t436 string
    var inline501 string = _goml_runtime_core_int32_to_string(t435)
    t436 = inline501
    println__T_string(t436)
    var t437 int32
    t437 = 40
    var t438 string
    var inline497 string = _goml_runtime_core_int32_to_string(t437)
    t438 = inline497
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline494)
    var t439 int32
    t439 = 40
    var t440 string
    var inline490 string = _goml_runtime_core_int32_to_string(t439)
    t440 = inline490
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline487)
    var t441 int32
    var inline485 int32 = 2
    switch inline485 {
    case 2:
        t441 = 90
    case 3:
        t441 = 100
    default:
        t441 = 100
    }
    var t442 string
    var inline483 string = _goml_runtime_core_int32_to_string(t441)
    t442 = inline483
    var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline480)
    var t443 int32
    var inline478 int32 = 3
    switch inline478 {
    case 2:
        t443 = 90
    case 3:
        t443 = 100
    default:
        t443 = 100
    }
    var t444 string
    var inline476 string = _goml_runtime_core_int32_to_string(t443)
    t444 = inline476
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline473)
    var t445 int32
    var inline471 int32 = 1
    switch inline471 {
    case 1:
        t445 = 60
    default:
        t445 = 80
    }
    var t446 string
    var inline469 string = _goml_runtime_core_int32_to_string(t445)
    t446 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline466)
    var t447 int32
    var inline464 int32 = 3
    switch inline464 {
    case 1:
        t447 = 60
    default:
        t447 = 80
    }
    var t448 string
    var inline462 string = _goml_runtime_core_int32_to_string(t447)
    t448 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline459)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t451 string
    t451 = value__1
    _goml_runtime_core_string_println(t451)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t455 string = _goml_runtime_core_int32_to_string(self__33)
    return t455
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
